package chowser.execute

import better.files.File
import chowser.util.genomics.{Location, VariantIdLocation}
import chowser.util.genomics.VariantIdLocation.VariantIdLocationTsvWriter

case class VariantComparerNew(idKey: String, chromosomeKey: String, positionKey: String) {

  def compare(file1: File, file2: File,
              fileToIter1: File => Iterator[VariantIdLocation], fileToIter2: File => Iterator[VariantIdLocation],
              inBothFileOpt: Option[File], inOneOnlyFileOpt: Option[File], inTwoOnlyFileOpt: Option[File]
             ): Unit = {
    val iter1 = fileToIter1(file1)
    val iter2 = fileToIter2(file2)
    val inBothSink = Sink.forFileOpt(inBothFileOpt)
    val inOneOnlySink = Sink.forFileOpt(inOneOnlyFileOpt)
    val inTwoOnlySink = Sink.forFileOpt(inTwoOnlyFileOpt)
    val buffer = VariantBuffer.apply(iter1, iter2, inBothSink, inOneOnlySink, inTwoOnlySink)
    val bufferAdvanced = buffer.advance
    ???
  }


  trait Sink {
    def write(variantIdLocation: VariantIdLocation): Unit
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
  }

  object NoOpSink extends Sink {
    override def write(variantIdLocation: VariantIdLocation): Unit = ()
  }

  class Channels(val iter1: Iterator[VariantIdLocation], val iter2: Iterator[VariantIdLocation],
                 val inBothSink: Sink, val inOneOnlySink: Sink, val inTwoOnlySink: Sink) {
    def nextOpt(iter: Iterator[VariantIdLocation]): Option[VariantIdLocation] = {
      if (iter.hasNext) Some(iter.next) else None
    }

    def nextOpt1: Option[VariantIdLocation] = nextOpt(iter1)

    def nextOpt2: Option[VariantIdLocation] = nextOpt(iter2)
  }

  trait VariantBuffer {
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

  class VariantBufferFlushed(val variantAheadOpt1: Option[VariantIdLocation],
                             val variantAheadOpt2: Option[VariantIdLocation],
                             val channels: Channels)
    extends VariantBuffer {

    def readVariantsAtLocation(location: Location, iter: Iterator[VariantIdLocation],
                               variantAheadOpt: Option[VariantIdLocation]):
    (Set[VariantIdLocation], Option[VariantIdLocation]) = {
       if(variantAheadOpt.nonEmpty) {
         var variantsHere = Set.empty
         while(variantAhead.location == location) {
           variantsHere += variantAhead
           variantAhead
         }
       } else {

       }
    }

    def advance: VariantBuffer = {
      val variantAheadNewOpt1 = variantAheadOpt1.orElse(channels.nextOpt1)
      val variantAheadNewOpt2 = variantAheadOpt1.orElse(channels.nextOpt2)
      (variantAheadNewOpt1, variantAheadNewOpt2) match {
        case (None, None) => new VariantBufferAllDone(channels)
        case (None, Some(variantAhead2)) => new VariantBufferDoneWith1(variantAhead2, channels)
        case (Some(variantAhead1), None) => new VariantBufferDoneWith2(variantAhead1, channels: Channels)
        case (Some(variantAhead1), Some(variantAhead2)) =>
          val location1 = variantAhead1.location
          val location2 = variantAhead2.location
          val location = Set(location1, location2).min
          val (variantsHere1, variantAheadOpt1) =
            if (location1 == location) {
              var variantsHere: Set[VariantIdLocation] = Set(variantAhead1)
              var variantAheadOpt = channels.nextOpt1
              while(variantAheadOpt.nonEmpty && variantAheadOpt.get.location == location) {
                variantsHere += variantAheadOpt.get
                variantAheadOpt = channels.nextOpt1
              }
              (variantsHere, variantAheadOpt)
            } else {
              (Set.empty, Some(variantAhead1))
            }
          ???
      }
    }

    override def variantsHere1: Set[VariantIdLocation] = Set.empty

    override def variantsHere2: Set[VariantIdLocation] = Set.empty
  }

  object VariantBufferFlushed {
    def apply(channels: Channels): VariantBufferFlushed =
      new VariantBufferFlushed(None, None, channels)
  }

  trait VariantBufferDoneWithSome extends VariantBuffer {
    override def variantsHere1: Set[VariantIdLocation] = Set.empty

    override def variantsHere2: Set[VariantIdLocation] = Set.empty
  }

  class VariantBufferDoneWith1(val variantAhead2: VariantIdLocation, val channels: Channels)
    extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantIdLocation] = None

    override def variantAheadOpt2: Option[VariantIdLocation] = Some(variantAhead2)

    def flush2: Unit = ???
  }

  class VariantBufferDoneWith2(val variantAhead1: VariantIdLocation, val channels: Channels)
    extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantIdLocation] = Some(variantAhead1)

    override def variantAheadOpt2: Option[VariantIdLocation] = None

    def flush1: Unit = ???
  }

  class VariantBufferAllDone(val channels: Channels) extends VariantBufferDoneWithSome {
    override def variantAheadOpt1: Option[VariantIdLocation] = None

    override def variantAheadOpt2: Option[VariantIdLocation] = None
  }

  class VariantBufferAtLocation(val location: Location,
                                val variantsHere1: Set[VariantIdLocation],
                                val variantsHere2: Set[VariantIdLocation],
                                val variantAheadOpt1: Option[VariantIdLocation],
                                val variantAheadOpt2: Option[VariantIdLocation])(val channels: Channels)
    extends VariantBuffer {
  }

}
