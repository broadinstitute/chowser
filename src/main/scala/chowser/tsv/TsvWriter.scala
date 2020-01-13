package chowser.tsv

import java.io.PrintWriter

import chowser.util.io.{OutputId, ResourceConfig}

class TsvWriter(file: OutputId, header: TsvHeader, resourceConfig: ResourceConfig) {
  val writer: PrintWriter = file.newPrintWriter(resourceConfig)
  for (headerLine <- header.lines) {
    writer.println(headerLine)
  }

  def addRow(row: TsvRow): Unit = writer.println(row.line)
}

object TsvWriter {
  def apply(file: OutputId, header: TsvHeader, resourceConfig: ResourceConfig): TsvWriter =
    new TsvWriter(file, header, resourceConfig)
}
