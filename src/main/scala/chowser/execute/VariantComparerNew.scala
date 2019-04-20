package chowser.execute

import better.files.File
import chowser.util.genomics.{Location, VariantIdLocation}
import chowser.util.genomics.Location.Ordering
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
    if (!iter1.hasNext) {
      iter2.foreach(inTwoOnlySink.write)
    } else if (!iter2.hasNext) {
      iter1.foreach(inOneOnlySink.write)
    } else {
      var variants1: Set[VariantIdLocation] = Set.empty
      var variants2: Set[VariantIdLocation] = Set.empty
      val currentVariant1 = iter1.next()
      val currentVariant2 = iter2.next()
      val currentLocation = Set(currentVariant1.location, currentVariant2.location).min
      val currentLocationVariants1: Set[VariantIdLocation] = Set.empty
      val currentLocationVariants2: Set[VariantIdLocation] = Set.empty
      val variantLaterLocationOpt1: Option[Location] = None
      val variantLaterLocationOpt2: Option[Location] = None
      val variantsAtCurrent1: Set[VariantIdLocation] = Set(currentVariant1)
    }
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
                 val inBothSink: Sink, val inOneOnlySink: Sink, val inTwoOnlySink: Sink)

  trait VariantBuffer {
    def hasLocation: Boolean = false

    def variantsHere1: Set[VariantIdLocation]

    def variantsHere2: Set[VariantIdLocation]

    def variantAheadOpt1: Option[VariantIdLocation]

    def variantAheadOpt2: Option[VariantIdLocation]

    def channels: Channels

    def isDoneWith1: Boolean = variantsHere1.isEmpty && variantAheadOpt1.isEmpty && channels.iter1.isEmpty

    def isDoneWith2: Boolean = variantsHere2.isEmpty && variantAheadOpt2.isEmpty && channels.iter2.isEmpty
  }


  object VariantBuffer {
    def apply(channels: Channels): VariantBufferFlushed = VariantBufferFlushed(channels)
  }

  case class VariantBufferFlushed(variantAheadOpt1: Option[VariantIdLocation],
                                  variantAheadOpt2: Option[VariantIdLocation])(val channels: Channels)
    extends VariantBuffer {
    def advance: VariantBuffer = {
      ??? // TODO
    }

    override def variantsHere1: Set[VariantIdLocation] = Set.empty

    override def variantsHere2: Set[VariantIdLocation] = Set.empty
  }

  object VariantBufferFlushed {
    def apply(channels: Channels): VariantBufferFlushed =
      VariantBufferFlushed(None, None)(channels)
  }

  case class VariantBufferAtLocation(location: Location,
                                     variantsHere1: Set[VariantIdLocation],
                                     variantsHere2: Set[VariantIdLocation],
                                     variantAheadOpt1: Option[VariantIdLocation],
                                     variantAheadOpt2: Option[VariantIdLocation])(val channels: Channels)
    extends VariantBuffer {
  }

}
