package chowser.tsv

import better.files.File
import chowser.tsv.TsvReader.{LineSplitter, Row}
import chowser.util.NumberParser

class TsvReader(val lineIterator: Iterator[String], val splitter: LineSplitter, val cols: Seq[String],
                val headerLines: Seq[String])
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

    def withValue(key: String, value: String): Row = {
      val valueMapNew = valueMap + (key -> value)
      val lineNew = cols.map(valueMapNew.getOrElse(_, "")).mkString("\t")
      copy(line = lineNew, valueMap = valueMapNew)
    }
  }

  trait LineCleaner {
    def clean(line: String): String
  }

  object LineCleaner {
    val noop: LineCleaner = (line: String) => line
    val removeLeadingSharps: LineCleaner = (line: String) => line.dropWhile(_ == '#')
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

  class LineParser(val headerCleaner: LineCleaner, val lineSplitter: LineSplitter) {
    def parseHeaderLine(line: String): Seq[String] = lineSplitter.split(headerCleaner.clean(line))
    def parseLine(line: String): Seq[String] = lineSplitter.split(line)
  }

  object LineParser {
    val default: LineParser = new LineParser(LineCleaner.removeLeadingSharps, LineSplitter.byTab)
  }

  def forSimpleHeaderLine(file: File, parser: LineParser = LineParser.default): TsvReader =
    forSimpleHeaderLine(file.lineIterator, parser)

  def forSimpleHeaderLine(lineIterator: Iterator[String], parser: LineParser): TsvReader = {
    val headerLine = lineIterator.next()
    val cols = parser.parseHeaderLine(headerLine)
    TsvReader(lineIterator, parser.lineSplitter, cols, Seq(headerLine))
  }

}
