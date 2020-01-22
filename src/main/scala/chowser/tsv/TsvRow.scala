package chowser.tsv

import chowser.util.NumberParser

case class TsvRow(line: String, cols: Seq[String], valueMap: Map[String, String]) {
  def string(colName: String): String = valueMap(colName)

  def unsignedInt(colName: String): Int = NumberParser.UnsignedIntParser.parse(valueMap(colName))

  def withValue(key: String, value: String): TsvRow = {
    val valueMapNew = valueMap + (key -> value)
    val lineNew = TsvRow.createLine(cols, valueMapNew)
    copy(line = lineNew, valueMap = valueMapNew)
  }
}

object TsvRow {
  def createLine(cols: Seq[String], valueMap: Map[String, String]): String =
    cols.map(valueMap.getOrElse(_, "")).mkString("\t")

  def apply(cols: Seq[String], valueMap: Map[String, String]): TsvRow = {
    val line = createLine(cols, valueMap)
    TsvRow(line, cols, valueMap)
  }

  def apply(values: Seq[String]): TsvRow = {
    val cols = (1 to values.size).map(_.toString)
    val valueMap = cols.zip(values).toMap
    TsvRow(cols, valueMap)
  }

  def apply(pair: (String, String), morePairs: (String, String)*): TsvRow = {
    val pairs = pair +: morePairs
    val cols = pairs.map(_._1)
    val valueMap = pairs.toMap
    TsvRow(cols, valueMap)
  }
}

