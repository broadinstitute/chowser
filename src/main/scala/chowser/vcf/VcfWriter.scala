package chowser.vcf

import better.files.File

class VcfWriter(val file: File, val header: VcfHeader) {

  if(file.nonEmpty) {
    file.clear()
  }
  header.lines.foreach(file.appendLine)

  def write(record: VcfRecord): Unit = file.appendLine(record.toLine)

}

object VcfWriter {
  def apply(file: File, header: VcfHeader): VcfWriter = new VcfWriter(file, header)
}
