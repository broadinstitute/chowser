package chowser.parser

import chowser.expressions.{Exit, Expression, FloatLiteral, IntLiteral, StringLiteral}
import fastparse._
import ScalaWhitespace._

object ChowserParser {

  def parseString(string: String): Either[String, Expression] = {
    parse(string, line(_)) match {
      case Parsed.Success(expression, _) => Right(expression)
      case failure: Parsed.Failure => Left(failure.msg + "\n" + failure.trace().longAggregateMsg)
    }
  }

  def line[_: P]: P[Expression] = P(Start ~ expression ~ End)

  def expression[_: P]: P[Expression] = P(exit | int | float | string)

  def exit[_: P]: P[Exit.type] = P("exit()".!).map(_ => Exit)

  def int[_: P]: P[IntLiteral] = P(CharPred(_.isDigit).rep(1).!).map(_.toLong).map(IntLiteral)

  def stringChars(c: Char): Boolean = c != '\"' && c != '\\'

  def digits[_: P]: P[Unit] = P(CharsWhileIn("0-9"))

  def exponent[_: P]: P[Unit] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)

  def fractional[_: P]: P[Unit] = P("." ~ digits)

  def integral[_: P]: P[Unit] = P("0" | CharIn("1-9") ~ digits.?)

  def float[_: P]: P[FloatLiteral] = P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
    x => FloatLiteral(x.toDouble)
  )

  def hexDigit[_: P]: P[Unit] = P(CharIn("0-9a-fA-F"))

  def unicodeEscape[_: P]: P[Unit] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)

  def escape[_: P]: P[Unit] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))

  def strChars[_: P]: P[Unit] = P(CharsWhile(stringChars))

  def string[_: P]: P[StringLiteral] = P("\"" ~/ (strChars | escape).rep.! ~ "\"").map(StringLiteral)
}
