package chowser.tsv

case class TsvHeader(lines: Seq[String])

object TsvHeader {
  def empty: TsvHeader = TsvHeader(Seq.empty)

  def ofColNames(cols: Seq[String]): TsvHeader = TsvHeader(Seq(cols.mkString("\t")))

  def ofLine(line: String): TsvHeader = TsvHeader(Seq(line))

  def ofLines(lines: Seq[String]): TsvHeader = TsvHeader(lines)

  def ofLinesThenColNames(lines: Seq[String], cols: Seq[String]): TsvHeader = TsvHeader(lines :+ cols.mkString("\t"))
}

