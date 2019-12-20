package chowser.tsv

import better.files.File

class TsvWriter(val file: File, header: TsvHeader) {
  if (file.nonEmpty) {
    file.clear()
  }
  for (headerLine <- header.lines) {
    file.appendLine(headerLine)
  }

  def addRow(row: TsvRow): Unit = file.appendLine(row.line)
}

object TsvWriter {
  def apply(file: File, header: TsvHeader): TsvWriter = new TsvWriter(file, header)
}
