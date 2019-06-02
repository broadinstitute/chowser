package chowser.parser.tokenize

case class Operator(string: String) {

}

object Operator {
  val chars: Set[Char] = "+-=*/\\&%$@^|!~".toSet
}
