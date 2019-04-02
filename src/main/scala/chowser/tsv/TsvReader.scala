package chowser.tsv

import better.files.File
import chowser.tsv.TsvReader.{LineSplitter, Row}
import chowser.util.NumberParser

class TsvReader(val lineIterator: Iterator[String], val splitter: LineSplitter, val cols: Seq[String],
                val headers: Seq[String])
  extends Iterator[Row] {
  override def hasNext: Boolean = lineIterator.hasNext

  override def next(): Row = {
    val line = lineIterator.next()
    val values = splitter.split(line)
    val valueMap = cols.zip(values).toMap
    Row(line, cols, valueMap)
  }
}

object TsvReader {

  def apply(lineIterator: Iterator[String], splitter: LineSplitter, cols: Seq[String],
            headers: Seq[String]): TsvReader =
    new TsvReader(lineIterator, splitter, cols, headers)

  case class Row(line: String, cols: Seq[String], valueMap: Map[String, String]) {
    def string(colName: String): String = valueMap(colName)

    def unsignedInt(colName: String): Int = NumberParser.UnsignedIntParser.parse(valueMap(colName))
  }

  trait LineSplitter {
    def split(line: String): Seq[String]
  }

  object LineSplitter {

    case class RegexSplitter(regex: String) extends LineSplitter {
      override def split(line: String): Seq[String] = line.split(regex)
    }

    val byTab: RegexSplitter = RegexSplitter("\t")
  }

  def forSimpleHeaderLine(file: File, splitter: LineSplitter = LineSplitter.byTab): TsvReader =
    forSimpleHeaderLine(file.lineIterator, splitter)

  def forSimpleHeaderLine(lineIterator: Iterator[String], splitter: LineSplitter): TsvReader = {
    val headerLine = lineIterator.next()
    val cols = splitter.split(headerLine)
    TsvReader(lineIterator, splitter, cols, Seq(headerLine))
  }

}
