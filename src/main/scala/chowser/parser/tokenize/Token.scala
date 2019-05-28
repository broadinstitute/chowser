package chowser.parser.tokenize

trait Token {
  def string: String

  def pos: Int

  def size: Int
}

object Token {

  case class WhiteSpace(string: String, pos: Int, size: Int) extends Token

  case class Identifier(string: String, pos: Int, size: Int) extends Token

  object Identifier {
    def apply(string: String, pos: Int): Identifier = Identifier(string, pos, string.size)
  }

  case class IntLiteral(string: String, value: Long, pos: Int, size: Int) extends Token

  case class FloatLiteral(string: String, value: Double, pos: Int, size: Int) extends Token

}