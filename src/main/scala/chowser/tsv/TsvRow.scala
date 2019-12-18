package chowser.tsv

import chowser.util.NumberParser

case class TsvRow(line: String, cols: Seq[String], valueMap: Map[String, String]) {
  def string(colName: String): String = valueMap(colName)

  def unsignedInt(colName: String): Int = NumberParser.UnsignedIntParser.parse(valueMap(colName))

  def withValue(key: String, value: String): TsvRow = {
    val valueMapNew = valueMap + (key -> value)
    val lineNew = cols.map(valueMapNew.getOrElse(_, "")).mkString("\t")
    copy(line = lineNew, valueMap = valueMapNew)
  }
}

