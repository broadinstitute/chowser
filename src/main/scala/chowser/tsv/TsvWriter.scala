package chowser.tsv

import better.files.File

class TsvWriter(val file: File, val headers: Seq[String]) {
  if(file.nonEmpty) {
    file.clear()
  }
  file.appendLine(valuesToLine(headers))

  def valuesToLine(values: Seq[String]): String = values.mkString("\t")

  def addRow(row: TsvRow): Unit = addRow(row.valueMap)

  def addRow(value: String, values: String*): Unit = addRow(value +: values)

  def addRow(values: Seq[String]): Unit = {
    file.appendLine(valuesToLine(values))
  }

  def addRow(valueMap: Map[String, String]): Unit = {
    addRow(headers.map(valueMap.getOrElse(_, "")))
  }

}

object TsvWriter {
  def apply(file: File, headers: Seq[String]): TsvWriter = new TsvWriter(file, headers)
}
