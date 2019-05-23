package chowser.vcf

import better.files.File

class VcfReader(val file: File, val header: VcfHeader, val recordsIterator: Iterator[VcfRecord]) {

}

object VcfReader {
  def apply(file:File): Either[String, VcfReader] = {
    val lineIterator = file.lineIterator
    VcfHeader.fromLineIterator(lineIterator) match {
      case Left(message) => Left(message)
      case Right(header) =>
        val recordsIterator = lineIterator.map(VcfRecord.fromLine)
        Right(new VcfReader(file, header, recordsIterator))
    }
  }
}