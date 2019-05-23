package chowser.vcf

import better.files.File

class VcfReader(file: File) {

  val lineIterator: Iterator[String] = file.lineIterator

  var headerLine: String = lineIterator.next()
  val (headerLines, recordsIterator) = {
???
  }

}
