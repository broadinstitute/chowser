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

  case class Operator(string: String, pos: Int, size: Int) extends Token

  object Operator {
    def apply(string: String, pos: Int): Operator = Operator(string, pos, string.size)
  }

  case class IntLiteral(string: String, value: Long, pos: Int, size: Int) extends Token

  case class FloatLiteral(string: String, value: Double, pos: Int, size: Int) extends Token

  case class StringLiteral(string: String, value: String, pos: Int, size: Int) extends Token

  trait SingleCharacterToken extends Token {
    def char: Char

    override def string = char.toString

    override def size: Int = 1
  }

  case class OpenParenthesis(pos: Int) extends SingleCharacterToken {
    override def char: Char = '('
  }

  case class CloseParenthesis(pos: Int) extends SingleCharacterToken {
    override def char: Char = ')'
  }

  case class OpenBrace(pos: Int) extends SingleCharacterToken {
    override def char: Char = '{'
  }

  case class CloseBrace(pos: Int) extends SingleCharacterToken {
    override def char: Char = '}'
  }

}