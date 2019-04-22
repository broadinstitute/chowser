package chowser.execute

import better.files.File
import chowser.util.genomics.{Location, VariantIdLocation}
import chowser.util.genomics.VariantIdLocation.VariantIdLocationTsvWriter

case class VariantComparer(idKey: String, chromosomeKey: String, positionKey: String) {

  def compare(file1: File, file2: File,
              fileToIter1: File => Iterator[VariantIdLocation], fileToIter2: File => Iterator[VariantIdLocation],
              inBothFileOpt: Option[File], inOneOnlyFileOpt: Option[File], inTwoOnlyFileOpt: Option[File]
             ): Unit = {
    val iter1 = fileToIter1(file1)
    val iter2 = fileToIter2(file2)
    val inBothSink = Sink.forFileOpt(inBothFileOpt)
    val inOneOnlySink = Sink.forFileOpt(inOneOnlyFileOpt)
    val inTwoOnlySink = Sink.forFileOpt(inTwoOnlyFileOpt)
    var buffer: VariantBuffer = VariantBuffer.apply(iter1, iter2, inBothSink, inOneOnlySink, inTwoOnlySink)
    while (!buffer.isTerminal) {
      buffer = buffer.next()
    }
  }


  trait Sink {
    def write(variantIdLocation: VariantIdLocation): Unit

    def writeAll(variantIdLocations: Iterable[VariantIdLocation]): Unit
  }

  object Sink {
    def forFileOpt(fileOpt: Option[File]): Sink = {
      fileOpt match {
        case Some(file) => FileSink(file)
        case None => NoOpSink
      }
    }
  }

  case class FileSink(file: File) extends Sink {
    val delegate = new VariantIdLocationTsvWriter(idKey, chromosomeKey, positionKey)(file)

    override def write(variantIdLocation: VariantIdLocation): Unit = delegate.add(variantIdLocation)

    override def writeAll(variantIdLocations: Iterable[VariantIdLocation]): Unit =
      variantIdLocations.toSeq.sortBy(_.id).foreach(delegate.add)
  }

  object NoOpSink extends Sink {
    override def write(variantIdLocation: VariantIdLocation): Unit = ()

    override def writeAll(variantIdLocations: Iterable[VariantIdLocation]): Unit = ()
  }

  final class Channels(val iter1: Iterator[VariantIdLocation], val iter2: Iterator[VariantIdLocation],
                               val inBothSink: Sink, val inOneOnlySink: Sink, val inTwoOnlySink: Sink) {
    def nextOpt(iter: Iterator[VariantIdLocation]): Option[VariantIdLocation] = {
      if (iter.hasNext) Some(iter.next) else None
    }

    def nextOpt1: Option[VariantIdLocation] = nextOpt(iter1)

    def nextOpt2: Option[VariantIdLocation] = nextOpt(iter2)
  }

  sealed trait VariantBuffer {
    def isTerminal: Boolean = false

    def next(): VariantBuffer

    def hasLocation: Boolean = false

    def variantsHere1: Set[VariantIdLocation]

    def variantsHere2: Set[VariantIdLocation]

    def variantAheadOpt1: Option[VariantIdLocation]

    def variantAheadOpt2: Option[VariantIdLocation]

    def channels: Channels
  }


  object VariantBuffer {
    def apply(iter1: Iterator[VariantIdLocation], iter2: Iterator[VariantIdLocation],
              inBothSink: Sink, inOneOnlySink: Sink, inTwoOnlySink: Sink): VariantBufferFlushed =
      apply(new Channels(iter1, iter2, inBothSink, inOneOnlySink, inTwoOnlySink))

    def apply(channels: Channels): VariantBufferFlushed = VariantBufferFlushed(channels)
  }

  final class VariantBufferFlushed(val variantAheadOpt1: Option[VariantIdLocation],
                                   val variantAheadOpt2: Option[VariantIdLocation],
                                   val channels: Channels)
    extends VariantBuffer {

    override def isTerminal: Boolean = false

    def readVariantsAtLocation(location: Location, variantAheadOpt: Option[VariantIdLocation],
                               iter: Iterator[VariantIdLocation]):
    (Set[VariantIdLocation], Option[VariantIdLocation]) = {
      var variantsHere: Set[VariantIdLocation] = Set.empty
      var variantAheadNewOpt: Option[VariantIdLocation] = variantAheadOpt
      var keepGoing = variantAheadNewOpt.nonEmpty
      while (keepGoing) {
        val variantAhead = variantAheadNewOpt.get
        if (variantAhead.location == location) {
          variantsHere += variantAhead
          variantAheadNewOpt = channels.nextOpt(iter)
          keepGoing = variantAheadNewOpt.nonEmpty
        } else {
          keepGoing = false
        }
      }
      (variantsHere, variantAheadNewOpt)
    }

    def advance: VariantBuffer = {
      val variantAheadNewOpt1 = variantAheadOpt1.orElse(channels.nextOpt1)
      val variantAheadNewOpt2 = variantAheadOpt1.orElse(channels.nextOpt2)
      (variantAheadNewOpt1, variantAheadNewOpt2) match {
        case (None, None) => new VariantBufferAllDone(channels)
        case (None, Some(variantAhead2)) => new VariantBufferDoneWith1(variantAhead2, channels)
        case (Some(variantAhead1), None) => new VariantBufferDoneWith2(variantAhead1, channels)
        case (Some(variantAhead1), Some(variantAhead2)) =>
          val location1 = variantAhead1.location
          val location2 = variantAhead2.location
          val location = Set(location1, location2).min
          val (variantsHere1, variantAheadOpt1) = readVariantsAtLocation(location, channels.nextOpt1, channels.iter1)
          val (variantsHere2, variantAheadOpt2) = readVariantsAtLocation(location, channels.nextOpt2, channels.iter2)
          new VariantBufferAtLocation(location, variantsHere1, variantsHere2, variantAheadOpt1,
            variantAheadOpt2, channels)
      }
    }

    override def variantsHere1: Set[VariantIdLocation] = Set.empty

    override def variantsHere2: Set[VariantIdLocation] = Set.empty

    override def next(): VariantBuffer = advance
  }

  object VariantBufferFlushed {
    def apply(channels: Channels): VariantBufferFlushed =
      new VariantBufferFlushed(None, None, channels)

    def apply(variantAheadOpt1: Option[VariantIdLocation],
              variantAheadOpt2: Option[VariantIdLocation], channels: Channels): VariantBufferFlushed =
      new VariantBufferFlushed(variantAheadOpt1, variantAheadOpt2, channels)
  }

  sealed trait VariantBufferDoneWithSome extends VariantBuffer {
    override def isTerminal: Boolean = false

    override def variantsHere1: Set[VariantIdLocation] = Set.empty

    override def variantsHere2: Set[VariantIdLocation] = Set.empty
  }

  final class VariantBufferDoneWith1(val variantAhead2: VariantIdLocation, val channels: Channels)
    extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantIdLocation] = None

    override def variantAheadOpt2: Option[VariantIdLocation] = Some(variantAhead2)

    def flush2(): Unit = {
      val write: VariantIdLocation => Unit = channels.inTwoOnlySink.write
      write(variantAhead2)
      channels.iter2.foreach(write)
    }

    override def next(): VariantBufferAllDone = {
      flush2()
      new VariantBufferAllDone(channels)
    }
  }

  final class VariantBufferDoneWith2(val variantAhead1: VariantIdLocation, val channels: Channels)
    extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantIdLocation] = Some(variantAhead1)

    override def variantAheadOpt2: Option[VariantIdLocation] = None

    def flush1(): Unit = {
      val write: VariantIdLocation => Unit = channels.inOneOnlySink.write
      write(variantAhead1)
      channels.iter1.foreach(write)
    }
    override def next(): VariantBufferAllDone = {
      flush1()
      new VariantBufferAllDone(channels)
    }
  }

  final class VariantBufferAllDone(val channels: Channels) extends VariantBufferDoneWithSome {
    override def isTerminal: Boolean = true

    override def variantAheadOpt1: Option[VariantIdLocation] = None

    override def variantAheadOpt2: Option[VariantIdLocation] = None

    override def next(): VariantBufferAllDone = this
  }

  final class VariantBufferAtLocation(val location: Location,
                                      val variantsHere1: Set[VariantIdLocation],
                                      val variantsHere2: Set[VariantIdLocation],
                                      val variantAheadOpt1: Option[VariantIdLocation],
                                      val variantAheadOpt2: Option[VariantIdLocation],
                                      val channels: Channels)
    extends VariantBuffer {
    override def isTerminal: Boolean = false

    def flush(): VariantBufferFlushed = {
      channels.inBothSink.writeAll(variantsHere1.intersect(variantsHere2))
      channels.inOneOnlySink.writeAll(variantsHere1 -- variantsHere2)
      channels.inTwoOnlySink.writeAll(variantsHere2 -- variantsHere1)
      VariantBufferFlushed(variantAheadOpt1, variantAheadOpt2, channels)
    }

    override def next(): VariantBufferFlushed = flush()
  }

}
