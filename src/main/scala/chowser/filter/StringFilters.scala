package chowser.filter

import java.util.regex.Pattern

import scala.util.control.NonFatal

object StringFilters {

  val doublePattern: Pattern = Pattern.compile("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")

  def parsesAsDouble(string: String): Boolean = {
    if (string == null) {
      false
    } else if (string.toLowerCase == "nan") {
      true
    } else {
      doublePattern.matcher(string).matches()
    }
  }

  def parsesAsDoubleFilter: Filter[String] = (string: String) => parsesAsDouble(string)

  case class StringAsDoubleFilter(doubleFilter: Filter[Double]) extends Filter[String] {
    override def apply(string: String): Boolean = {
      if (parsesAsDouble(string)) {
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
