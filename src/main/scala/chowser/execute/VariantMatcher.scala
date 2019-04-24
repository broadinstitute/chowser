package chowser.execute

import better.files.File
import chowser.genomics.VariantId.VariantIdTsvWriter
import chowser.genomics.{Location, VariantId}

case class VariantMatcher(idKey: String) {

  def compare(file1: File, file2: File,
              fileToIter1: File => Iterator[VariantId], fileToIter2: File => Iterator[VariantId],
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
    def write(variantIdLocation: VariantId): Unit

    def writeAll(variantIdLocations: Iterable[VariantId]): Unit
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
    val delegate = new VariantIdTsvWriter(idKey)(file)

    override def write(variantIdLocation: VariantId): Unit = delegate.add(variantIdLocation)

    override def writeAll(variantIdLocations: Iterable[VariantId]): Unit = variantIdLocations.foreach(delegate.add)
  }

  object NoOpSink extends Sink {
    override def write(variantIdLocation: VariantId): Unit = ()

    override def writeAll(variantIdLocations: Iterable[VariantId]): Unit = ()
  }

  final class Channels(val iter1: Iterator[VariantId], val iter2: Iterator[VariantId],
                       val inBothSink: Sink, val inOneOnlySink: Sink, val inTwoOnlySink: Sink) {
    def nextOpt(iter: Iterator[VariantId]): Option[VariantId] = {
      if (iter.hasNext) Some(iter.next) else None
    }

    def nextOpt1: Option[VariantId] = nextOpt(iter1)

    def nextOpt2: Option[VariantId] = nextOpt(iter2)
  }

  sealed trait VariantBuffer {
    def isTerminal: Boolean = false

    def next(): VariantBuffer

    def hasLocation: Boolean = false

    def variantsHere1: Set[VariantId]

    def variantsHere2: Set[VariantId]

    def variantAheadOpt1: Option[VariantId]

    def variantAheadOpt2: Option[VariantId]

    def channels: Channels
  }


  object VariantBuffer {
    def apply(iter1: Iterator[VariantId], iter2: Iterator[VariantId],
              inBothSink: Sink, inOneOnlySink: Sink, inTwoOnlySink: Sink): VariantBufferFlushed =
      apply(new Channels(iter1, iter2, inBothSink, inOneOnlySink, inTwoOnlySink))

    def apply(channels: Channels): VariantBufferFlushed = VariantBufferFlushed(channels)
  }

  final class VariantBufferFlushed(val variantAheadOpt1: Option[VariantId],
                                   val variantAheadOpt2: Option[VariantId],
                                   val channels: Channels)
    extends VariantBuffer {

    override def isTerminal: Boolean = false

    def readVariantsAtLocation(location: Location, variantAhead: VariantId,
                               iter: Iterator[VariantId]):
    (Set[VariantId], Option[VariantId]) = {
      var variantsHere: Set[VariantId] = Set.empty
      var variantAheadNewOpt: Option[VariantId] = Some(variantAhead)
      var keepGoing = variantAheadNewOpt.nonEmpty
      while (keepGoing) {
        val variantAheadNew = variantAheadNewOpt.get
        if (variantAheadNew.location == location) {
          variantsHere += variantAheadNew
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
      val variantAheadNewOpt2 = variantAheadOpt2.orElse(channels.nextOpt2)
      (variantAheadNewOpt1, variantAheadNewOpt2) match {
        case (None, None) => new VariantBufferAllDone(channels)
        case (None, Some(variantAhead2)) => new VariantBufferDoneWith1(variantAhead2, channels)
        case (Some(variantAhead1), None) => new VariantBufferDoneWith2(variantAhead1, channels)
        case (Some(variantAhead1), Some(variantAhead2)) =>
          val location1 = variantAhead1.location
          val location2 = variantAhead2.location
          val location = Set(location1, location2).min
          val (variantsHere1, variantAheadOpt1) = readVariantsAtLocation(location, variantAhead1, channels.iter1)
          val (variantsHere2, variantAheadOpt2) = readVariantsAtLocation(location, variantAhead2, channels.iter2)
          new VariantBufferAtLocation(location, variantsHere1, variantsHere2, variantAheadOpt1,
            variantAheadOpt2, channels)
      }
    }

    override def variantsHere1: Set[VariantId] = Set.empty

    override def variantsHere2: Set[VariantId] = Set.empty

    override def next(): VariantBuffer = advance

    override def toString: String = s"VariantBufferFlushed($variantAheadOpt1, $variantAheadOpt2)"
  }

  object VariantBufferFlushed {
    def apply(channels: Channels): VariantBufferFlushed =
      new VariantBufferFlushed(None, None, channels)

    def apply(variantAheadOpt1: Option[VariantId],
              variantAheadOpt2: Option[VariantId], channels: Channels): VariantBufferFlushed =
      new VariantBufferFlushed(variantAheadOpt1, variantAheadOpt2, channels)
  }

  sealed trait VariantBufferDoneWithSome extends VariantBuffer {
    override def isTerminal: Boolean = false

    override def variantsHere1: Set[VariantId] = Set.empty

    override def variantsHere2: Set[VariantId] = Set.empty
  }

  final class VariantBufferDoneWith1(val variantAhead2: VariantId, val channels: Channels)
    extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantId] = None

    override def variantAheadOpt2: Option[VariantId] = Some(variantAhead2)

    def flush2(): Unit = {
      val write: VariantId => Unit = channels.inTwoOnlySink.write
      write(variantAhead2)
      channels.iter2.foreach(write)
    }

    override def next(): VariantBufferAllDone = {
      flush2()
      new VariantBufferAllDone(channels)
    }
  }

  final class VariantBufferDoneWith2(val variantAhead1: VariantId, val channels: Channels)
    extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantId] = Some(variantAhead1)

    override def variantAheadOpt2: Option[VariantId] = None

    def flush1(): Unit = {
      val write: VariantId => Unit = channels.inOneOnlySink.write
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

    override def variantAheadOpt1: Option[VariantId] = None

    override def variantAheadOpt2: Option[VariantId] = None

    override def next(): VariantBufferAllDone = this
  }

  final class VariantBufferAtLocation(val location: Location,
                                      val variantsHere1: Set[VariantId],
                                      val variantsHere2: Set[VariantId],
                                      val variantAheadOpt1: Option[VariantId],
                                      val variantAheadOpt2: Option[VariantId],
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

    override def toString: String =
      s"VariantBufferAtLocation($location, $variantsHere1, $variantsHere2, $variantAheadOpt1, $variantAheadOpt2)"
  }

}
