package chowser.interpreter.tokenize

case class Brackets(charOpen: Char, charClose: Char) {
  def openBracket: OpenBracket = OpenBracket(this)

  def closeBracket: CloseBracket = CloseBracket(this)
}

case class OpenBracket(bracket: Brackets) {
  def char: Char = bracket.charOpen

  def closer: CloseBracket = bracket.closeBracket
}

case class CloseBracket(bracket: Brackets) {
  def char: Char = bracket.charClose

  def opener: OpenBracket = bracket.openBracket
}

object Brackets {

  val all: Set[Brackets] = Set(Parentheses, Braces)

  object Parentheses extends Brackets('(', ')')

  object Braces extends Brackets('{', '}')

}

object OpenBracket {
  val all: Set[OpenBracket] = Brackets.all.map(_.openBracket)
  val chars: Set[Char] = all.map(_.char)
  val charToOpenBracket: Map[Char, OpenBracket] = all.map(ob => (ob.char, ob)).toMap
}

object CloseBracket {
  val all: Set[CloseBracket] = Brackets.all.map(_.closeBracket)
  val chars: Set[Char] = all.map(_.char)
  val charToCloseBracket: Map[Char, CloseBracket] = all.map(ob => (ob.char, ob)).toMap
}