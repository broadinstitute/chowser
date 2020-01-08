package chowser.util

import java.util.regex.Pattern

import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.{Failure, Success, Try}

trait NumberParser[T] {

  def regex: String

  def pattern: Pattern = Pattern.compile(regex)

  def isValid(string: String): Boolean = pattern.matcher(string).matches()

  def parse(string: String): T

  def parseOpt(string: String): Option[T] = {
    if (isValid(string)) {
      Try {
        parse(string)
      }.toOption
    } else {
      None
    }
  }

  def parseEither(string: String): Either[Snag, T] = {
    if (isValid(string)) {
      Try {
        parse(string)
      } match {
        case Success(value) => Right(value)
        case Failure(exception) => Left(Snag(exception))
      }
    } else {
      Left(Snag(s"$string is not a valid number."))
    }
  }


}

object NumberParser {

  object DoubleParser extends NumberParser[Double] {
    override def regex: String = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"

    override def parse(string: String): Double = string.toDouble
  }

  object UnsignedIntParser extends NumberParser[Int] {
    override def regex: String = "[0-9]+"

    override def parse(string: String): Int = string.toInt
  }

  object LongParser extends NumberParser[Long] {
    override def regex: String = "[+-]?[0-9]+"

    override def parse(string: String): Long = string.toLong
  }

}