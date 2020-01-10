package chowser.tsv

import chowser.util.io.OutputId

class TsvWriter(val file: OutputId, header: TsvHeader) {
  if (file.file.nonEmpty) {
    file.file.clear()
  }
  for (headerLine <- header.lines) {
    file.file.appendLine(headerLine)
  }

  def addRow(row: TsvRow): Unit = file.file.appendLine(row.line)
}

object TsvWriter {
  def apply(file: OutputId, header: TsvHeader): TsvWriter = new TsvWriter(file, header)
}
