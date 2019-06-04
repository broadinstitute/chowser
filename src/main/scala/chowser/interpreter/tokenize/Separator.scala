package chowser.interpreter.tokenize

case class Separator(char: Char) {

}

object Separator {
  val all: Set[Separator] = Set(Colon, Semicolon, Dot, Comma)
  val chars: Set[Char] = all.map(_.char)
  val charToSeparator: Map[Char, Separator] = all.map(sep => (sep.char, sep)).toMap

  object Colon extends Separator(':')

  object Semicolon extends Separator(';')

  object Dot extends Separator('.')

  object Comma extends Separator(',')

}