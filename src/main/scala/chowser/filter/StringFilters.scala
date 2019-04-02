package chowser.filter

import chowser.util.NumberParser

import scala.util.control.NonFatal

object StringFilters {

  def parsesAsDoubleFilter: Filter[String] = (string: String) => NumberParser.DoubleParser.isValid(string)

  def parsesAsUnsignedIntegerFilter: Filter[String] =
    (string: String) => NumberParser.UnsignedIntParser.isValid(string)

  case class StringAsDoubleFilter(doubleFilter: Filter[Double]) extends Filter[String] {
    override def apply(string: String): Boolean = {
      if (NumberParser.DoubleParser.isValid(string)) {
        try {
          doubleFilter(string.toDouble)
        } catch {
          case NonFatal(_) => false
        }
      } else {
        false
      }
    }

  }

  def parsesAsDoubleAndFilter(doubleFilter: Filter[Double]): Filter[String] = StringAsDoubleFilter(doubleFilter)

}
