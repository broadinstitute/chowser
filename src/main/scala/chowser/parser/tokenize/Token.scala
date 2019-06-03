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

    def precedence: Int = operator.precedence
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

  trait CompositeToken extends Token {
    def children: Seq[Token]

    override def string: String = children.map(_.string).mkString("")

    override def size: Int = children.map(_.size).sum

    override def pos: Int = children.head.pos
  }

  case class UnaryOpToken(op: OperatorToken, term: TermToken) extends TermToken with CompositeToken {
    override def children: Seq[Token] = Seq(op, term)
  }

  case class BinaryOpToken(lhs: TermToken, op: OperatorToken, rhs: TermToken) extends TermToken with CompositeToken {
    override def children: Seq[Token] = Seq(lhs, op, rhs)
  }

  case class BracketedTermToken(open: OpenBracketToken, term: TermToken, close: CloseBracketToken)
    extends TermToken with CompositeToken {
    override def children: Seq[Token] = Seq(open, term, close)
  }

}