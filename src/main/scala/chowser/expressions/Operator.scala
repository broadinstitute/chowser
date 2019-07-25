package chowser.expressions

case class Operator(string: String) {
  def canBeUnaryPrefix: Boolean = Operator.canBeUnaryPrefix(string)

  def isAssignment: Boolean = Operator.isAssignment(string)

  def precedence: Int = Operator.precedence(string)
}

object Operator {
  val chars: Set[Char] = "+-=*/\\&%$@^|!~".toSet

  val unaryPrefixOpInitials: Set[Char] = "+-~!".toSet

  def canBeUnaryPrefix(string: String): Boolean = unaryPrefixOpInitials(string.charAt(0))

  def isAssignment(string: String): Boolean =
    (string.last == '=') && !(string.size > 1 && string.charAt(0) == '=') &&
      string != "<=" && string != ">=" && string != "!="

  def precedence(string: String): Int = {
    if (isAssignment(string)) {
      0
    } else {
      string.charAt(0) match {
        case '|' => 1
        case '^' => 2
        case '&' => 3
        case '=' | '!' => 4
        case '<' | '>' => 5
        case ':' => 6
        case '+' | '-' => 7
        case '*' | '/' | '%' => 8
        case _ => 9
      }
    }
  }
}
