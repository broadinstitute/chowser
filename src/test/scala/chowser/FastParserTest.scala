package chowser

import fastparse._, NoWhitespace._

object FastParserTest extends App {

  def term[_:P]: P[Unit] = P("a")

  def sum[_:P]: P[Unit] = P(term ~ "+" ~ term)

  def expression[_:P]: P[Unit] = P(term|sum)

  def line[_:P]: P[Unit] = P(Start ~ expression ~ End)

  val result: Parsed[Unit] = parse("a+a", line(_))

  println(result)

}
