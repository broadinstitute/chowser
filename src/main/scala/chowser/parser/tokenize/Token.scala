package chowser.parser.tokenize

trait Token {
  def string: String

  def pos: Int

  def size: Int
}

object Token {

  case class Identifier(string: String, pos: Int, size: Int) extends Token

  object Identifier {
    def apply(string: String, pos: Int): Identifier = Identifier(string, pos, string.size)
  }

}