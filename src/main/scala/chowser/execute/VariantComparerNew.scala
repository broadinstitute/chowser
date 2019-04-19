package chowser.execute

import better.files.File
import chowser.util.genomics.VariantIdLocation
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

}
