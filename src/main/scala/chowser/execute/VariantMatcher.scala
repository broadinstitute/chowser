package chowser.execute

import better.files.File
import chowser.genomics.VariantGroupId.VariantGroupIdTsvWriter
import chowser.genomics.{Location, VariantGroupId}
import chowser.util.IntersecterAndDiffer

case class VariantMatcher(idKey: String) {

  def compare(file1: File, file2: File,
              fileToIter1: File => Iterator[VariantGroupId], fileToIter2: File => Iterator[VariantGroupId],
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
    def write(variantGroupId: VariantGroupId): Unit

    def writeAll(variantGroupIds: Iterable[VariantGroupId]): Unit
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
    val delegate = new VariantGroupIdTsvWriter(idKey)(file)

    override def write(variantGroupId: VariantGroupId): Unit = delegate.add(variantGroupId)

    override def writeAll(variantGroupId: Iterable[VariantGroupId]): Unit = variantGroupId.foreach(delegate.add)
  }

  object NoOpSink extends Sink {
    override def write(variantIdLocation: VariantGroupId): Unit = ()

    override def writeAll(variantIdLocations: Iterable[VariantGroupId]): Unit = ()
  }

  final class Channels(val iter1: Iterator[VariantGroupId], val iter2: Iterator[VariantGroupId],
                       val inBothSink: Sink, val inOneOnlySink: Sink, val inTwoOnlySink: Sink) {
    def nextOpt(iter: Iterator[VariantGroupId]): Option[VariantGroupId] = {
      if (iter.hasNext) Some(iter.next) else None
    }

    def nextOpt1: Option[VariantGroupId] = nextOpt(iter1)

    def nextOpt2: Option[VariantGroupId] = nextOpt(iter2)
  }

  sealed trait VariantBuffer {
    def isTerminal: Boolean = false

    def next(): VariantBuffer

    def hasLocation: Boolean = false

    def variantsHere1: Set[VariantGroupId]

    def variantsHere2: Set[VariantGroupId]

    def variantAheadOpt1: Option[VariantGroupId]

    def variantAheadOpt2: Option[VariantGroupId]

    def channels: Channels
  }


  object VariantBuffer {
    def apply(iter1: Iterator[VariantGroupId], iter2: Iterator[VariantGroupId],
              inBothSink: Sink, inOneOnlySink: Sink, inTwoOnlySink: Sink): VariantBufferFlushed =
      apply(new Channels(iter1, iter2, inBothSink, inOneOnlySink, inTwoOnlySink))

    def apply(channels: Channels): VariantBufferFlushed = VariantBufferFlushed(channels)
  }

  final class VariantBufferFlushed(val variantAheadOpt1: Option[VariantGroupId],
                                   val variantAheadOpt2: Option[VariantGroupId],
                                   val channels: Channels)
    extends VariantBuffer {

    override def isTerminal: Boolean = false

    def readVariantsAtLocation(location: Location, variantAhead: VariantGroupId,
                               iter: Iterator[VariantGroupId]):
    (Set[VariantGroupId], Option[VariantGroupId]) = {
      var variantsHere: Set[VariantGroupId] = Set.empty
      var variantAheadNewOpt: Option[VariantGroupId] = Some(variantAhead)
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

    override def variantsHere1: Set[VariantGroupId] = Set.empty

    override def variantsHere2: Set[VariantGroupId] = Set.empty

    override def next(): VariantBuffer = advance

    override def toString: String = s"VariantBufferFlushed($variantAheadOpt1, $variantAheadOpt2)"
  }

  object VariantBufferFlushed {
    def apply(channels: Channels): VariantBufferFlushed =
      new VariantBufferFlushed(None, None, channels)

    def apply(variantAheadOpt1: Option[VariantGroupId],
              variantAheadOpt2: Option[VariantGroupId], channels: Channels): VariantBufferFlushed =
      new VariantBufferFlushed(variantAheadOpt1, variantAheadOpt2, channels)
  }

  sealed trait VariantBufferDoneWithSome extends VariantBuffer {
    override def isTerminal: Boolean = false

    override def variantsHere1: Set[VariantGroupId] = Set.empty

    override def variantsHere2: Set[VariantGroupId] = Set.empty
  }

  final class VariantBufferDoneWith1(val variantAhead2: VariantGroupId, val channels: Channels)
    extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantGroupId] = None

    override def variantAheadOpt2: Option[VariantGroupId] = Some(variantAhead2)

    def flush2(): Unit = {
      val write: VariantGroupId => Unit = channels.inTwoOnlySink.write
      write(variantAhead2)
      channels.iter2.foreach(write)
    }

    override def next(): VariantBufferAllDone = {
      flush2()
      new VariantBufferAllDone(channels)
    }
  }

  final class VariantBufferDoneWith2(val variantAhead1: VariantGroupId, val channels: Channels)
    extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantGroupId] = Some(variantAhead1)

    override def variantAheadOpt2: Option[VariantGroupId] = None

    def flush1(): Unit = {
      val write: VariantGroupId => Unit = channels.inOneOnlySink.write
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

    override def variantAheadOpt1: Option[VariantGroupId] = None

    override def variantAheadOpt2: Option[VariantGroupId] = None

    override def next(): VariantBufferAllDone = this
  }

  final class VariantBufferAtLocation(val location: Location,
                                      val variantsHere1: Set[VariantGroupId],
                                      val variantsHere2: Set[VariantGroupId],
                                      val variantAheadOpt1: Option[VariantGroupId],
                                      val variantAheadOpt2: Option[VariantGroupId],
                                      val channels: Channels)
    extends VariantBuffer {
    override def isTerminal: Boolean = false

    def flush(): VariantBufferFlushed = {
      val intersectionAndDiffs =
        IntersecterAndDiffer.intersectAndDiffByKey(variantsHere1, variantsHere2,
          (id: VariantGroupId) => id.alleles.toSet)
      channels.inBothSink.writeAll(intersectionAndDiffs.intersection)
      channels.inOneOnlySink.writeAll(intersectionAndDiffs.diff1)
      channels.inTwoOnlySink.writeAll(intersectionAndDiffs.diff2)
      VariantBufferFlushed(variantAheadOpt1, variantAheadOpt2, channels)
    }

    override def next(): VariantBufferFlushed = flush()

    override def toString: String =
      s"VariantBufferAtLocation($location, $variantsHere1, $variantsHere2, $variantAheadOpt1, $variantAheadOpt2)"
  }

}
