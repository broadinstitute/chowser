package chowser.tsv

import better.files.File
import chowser.tsv.BasicTsvReader.LineParser

class BasicTsvReader(val lineIterator: Iterator[String], val parser: LineParser, val cols: Seq[String],
                     val header: TsvHeader)
  extends TsvReader {
  override def hasNext: Boolean = lineIterator.hasNext

  override def next(): TsvRow = {
    val line = lineIterator.next()
    val values = parser.parseRecordLine(line)
    val valueMap = cols.zip(values).toMap
    TsvRow(line, cols, valueMap)
  }
}

object BasicTsvReader {

  def apply(lineIterator: Iterator[String], parser: LineParser, cols: Seq[String],
            header: TsvHeader): BasicTsvReader =
    new BasicTsvReader(lineIterator, parser, cols, header)

  trait LineCleaner {
    def clean(line: String): String
  }

  object LineCleaner {
    val noop: LineCleaner = (line: String) => line
    val trim: LineCleaner = (line: String) => line.trim
    val removeLeadingSharps: LineCleaner = (line: String) => line.dropWhile(_ == '#')
    val trimAndRemoveLeadingSharps: LineCleaner = (line: String) => line.trim.dropWhile(_ == '#')
  }

  trait LineSplitter {
    def split(line: String): Seq[String]
  }

  object LineSplitter {

    case class RegexSplitter(regex: String) extends LineSplitter {
      override def split(line: String): Seq[String] = line.split(regex)
    }

    val byTab: RegexSplitter = RegexSplitter("\t")
    val byWhiteSpaceGroup: RegexSplitter = RegexSplitter("\\s+")
  }

  class LineParser(val headerCleaner: LineCleaner, val recordCleaner: LineCleaner, val lineSplitter: LineSplitter) {
    def parseHeaderLine(line: String): Seq[String] = lineSplitter.split(headerCleaner.clean(line))
    def parseRecordLine(line: String): Seq[String] = lineSplitter.split(recordCleaner.clean(line))
  }

  object LineParser {
    val default: LineParser = new LineParser(LineCleaner.removeLeadingSharps, LineCleaner.noop, LineSplitter.byTab)
    val whitespace: LineParser = new LineParser(LineCleaner.trim, LineCleaner.trim, LineSplitter.byWhiteSpaceGroup)
  }

  def forSimpleHeaderLine(file: File, parser: LineParser = LineParser.default): BasicTsvReader =
    forSimpleHeaderLine(file.lineIterator, parser)

  def forSimpleHeaderLine(lineIterator: Iterator[String], parser: LineParser): BasicTsvReader = {
    val headerLine = lineIterator.next()
    val cols = parser.parseHeaderLine(headerLine)
    BasicTsvReader(lineIterator, parser, cols, TsvHeader.ofLine(headerLine))
  }

}
