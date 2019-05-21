package chowser.parser

import chowser.expressions.{Exit, Expression, IntLiteral}
import fastparse._
import ScalaWhitespace._

object ChowserParser {

  def parseString(string: String): Either[String, Expression] = {
    parse(string, expression(_)) match {
      case Parsed.Success(expression, _) => Right(expression)
      case failure: Parsed.Failure => Left(failure.msg + "\n" + failure.trace().longAggregateMsg)
    }
  }

  def line[_:P]: P[Expression] = P(Start ~ expression ~ End)
  def expression[_:P]: P[Expression] = P(exit | int)
  def exit[_: P]: P[Exit.type] = P("exit()".!).map(_ => Exit)
  def int[_:P]: P[IntLiteral] = P(CharPred(_.isDigit).rep(1).!).map(_.toLong).map(IntLiteral)

  def stringChars(c: Char) = c != '\"' && c != '\\'

  def digits[_: P]        = P( CharsWhileIn("0-9") )
  def exponent[_: P]      = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
  def fractional[_: P]    = P( "." ~ digits )
  def integral[_: P]      = P( "0" | CharIn("1-9")  ~ digits.? )

  def float[_: P] = P(  CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => Js.Num(x.toDouble)
  )

  def hexDigit[_: P]      = P( CharIn("0-9a-fA-F") )
  def unicodeEscape[_: P] = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  def escape[_: P]        = P( "\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )

  def strChars[_: P] = P( CharsWhile(stringChars) )
  def string[_: P] =
    P("\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)
}
