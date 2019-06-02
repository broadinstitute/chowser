package chowser.parser.tokenize

import chowser.expressions.Expression

sealed trait Token {
  def string: String

  def pos: Int

  def size: Int
}

object Token {
  sealed trait TermToken extends Token

  case class WhiteSpace(string: String, pos: Int, size: Int) extends Token

  case class Identifier(string: String, pos: Int, size: Int) extends TermToken

  object Identifier {
    def apply(string: String, pos: Int): Identifier = Identifier(string, pos, string.size)
  }

  case class OperatorToken(operator: Operator, pos: Int) extends Token {
    override def string: String = operator.string

    override def size: Int = string.size
  }

  case class ExpressionToken(string: String, expression: Expression, pos: Int, size: Int) extends TermToken

  sealed trait SingleCharacterToken extends Token {
    def char: Char

    override def string: String = char.toString

    override def size: Int = 1
  }

  case class OpenBracketToken(openBracket: OpenBracket, pos: Int) extends SingleCharacterToken {
    override def char: Char = openBracket.char
  }

  case class CloseBracketToken(closeBracket: CloseBracket, pos: Int) extends SingleCharacterToken {
    override def char: Char = closeBracket.char
  }

  case class SeparatorToken(separator: Separator, pos: Int) extends SingleCharacterToken {
    override def char: Char = separator.char
  }

  case class UnaryOpToken(op: OperatorToken, term: TermToken) extends TermToken {
    override def string: String = op.string + term.string

    override def pos: Int = op.pos

    override def size: Int = op.size + term.size
  }

  case class BinaryOpToken(lhs: TermToken, op: OperatorToken, rhs: TermToken) extends TermToken {
    override def string: String = lhs.string + op.string + rhs.string

    override def pos: Int = lhs.pos

    override def size: Int = lhs.size + op.size + rhs.size
  }

}