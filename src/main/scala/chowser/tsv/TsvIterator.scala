package chowser.tsv

import chowser.tsv.TsvIterator.{LineSplitter, Row}

class TsvIterator(val lineIterator: Iterator[String], val splitter: LineSplitter, val cols: Seq[String],
                  val headers: Seq[String])
  extends Iterator[Row]{
  override def hasNext: Boolean = lineIterator.hasNext

  override def next(): Row = {
    val line = lineIterator.next()
    val values = splitter.split(line)
    val valueMap = cols.zip(values).toMap
    Row(line, cols, valueMap)
  }
}

object TsvIterator {

  def apply(lineIterator: Iterator[String], splitter: LineSplitter, cols: Seq[String],
            headers: Seq[String]): TsvIterator =
    new TsvIterator(lineIterator, splitter, cols, headers)

  case class Row(line: String, cols: Seq[String], valueMap: Map[String, String])

  trait LineSplitter {
    def split(line: String): Seq[String]
  }

  object LineSplitter {

    case class RegexSplitter(regex: String) extends LineSplitter {
      override def split(line: String): Seq[String] = line.split(regex)
    }

    val byTab: RegexSplitter = RegexSplitter("\t")
  }

  def forSimpleHeaderLine(lineIterator: Iterator[String], splitter: LineSplitter): TsvIterator = {
    val headerLine = lineIterator.next()
    val cols = splitter.split(headerLine)
    TsvIterator(lineIterator, splitter, cols, Seq(headerLine))
  }

}
