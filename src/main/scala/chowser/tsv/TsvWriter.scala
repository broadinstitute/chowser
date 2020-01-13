package chowser.tsv

import chowser.util.io.OutputId

class TsvWriter(val file: OutputId, header: TsvHeader) {
  if (file.fileDeprecated.nonEmpty) {
    file.fileDeprecated.clear()
  }
  for (headerLine <- header.lines) {
    file.fileDeprecated.appendLine(headerLine)
  }

  def addRow(row: TsvRow): Unit = file.fileDeprecated.appendLine(row.line)
}

object TsvWriter {
  def apply(file: OutputId, header: TsvHeader): TsvWriter = new TsvWriter(file, header)
}
