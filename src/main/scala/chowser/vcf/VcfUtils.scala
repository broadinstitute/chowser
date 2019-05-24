package chowser.vcf

import better.files.File

object VcfUtils {
  def transformVcf(inFile: File, outFile: File)(transformer: Iterator[VcfRecord] => Iterator[VcfRecord]): Unit = {
    VcfReader(inFile) match {
      case Right(reader) =>
        val writer = VcfWriter(outFile, reader.header)
        transformer(reader.recordsIterator).foreach(writer.write)
      case Left(message) => println(message)
    }
  }

}
