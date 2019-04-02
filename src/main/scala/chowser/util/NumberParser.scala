package chowser.util

import java.util.regex.Pattern

import scala.util.Try

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

}