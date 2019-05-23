package chowser.vcf

import better.files.File

class VcfWriter(file: File, header: VcfHeader) {

  if(file.nonEmpty) {
    file.clear()
  }
  header.lines.foreach(file.appendLine)

  def write(record: VcfRecord): Unit = file.appendLine(record.toLine)

}
