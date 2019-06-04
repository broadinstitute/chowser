package chowser.parser.tokenize

import chowser.expressions.Expression

sealed trait Token {
  def string: String

  def pos: Int

  def size: Int
}

object Token {

  sealed trait TermToken extends Token

  case class WhiteSpaceToken(string: String, pos: Int, size: Int) extends Token

  sealed trait CallableToken extends TermToken

  case class IdentifierToken(string: String, pos: Int, size: Int) extends CallableToken

  object IdentifierToken {
    def apply(string: String, pos: Int): IdentifierToken = IdentifierToken(string, pos, string.size)
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

  sealed trait OpenBracketToken extends SingleCharacterToken {
    def open: OpenBracket

    override def char: Char = open.char
  }

  case class OpenParenToken(pos: Int) extends OpenBracketToken {
    override def open: OpenBracket = Brackets.Parentheses.openBracket
  }

  case class OpenBraceToken(pos: Int) extends OpenBracketToken {
    override def open: OpenBracket = Brackets.Braces.openBracket
  }

  sealed trait CloseBracketToken extends SingleCharacterToken {
    def close: CloseBracket

    override def char: Char = close.char
  }

  case class CloseParenToken(pos: Int) extends CloseBracketToken {
    override def close: CloseBracket = Brackets.Parentheses.closeBracket
  }

  case class CloseBraceToken(pos: Int) extends CloseBracketToken {
    override def close: CloseBracket = Brackets.Braces.closeBracket
  }

  case class OpenBracketTokenOld(openBracket: OpenBracket, pos: Int) extends SingleCharacterToken {
    override def char: Char = openBracket.char
  }

  case class CloseBracketTokenOld(closeBracket: CloseBracket, pos: Int) extends SingleCharacterToken {
    override def char: Char = closeBracket.char
  }

  case class DefinitionToken(pos: Int) extends Token {
    override def string: String = ":="

    override def size: Int = 2
  }

  case class CommaToken(pos: Int) extends SingleCharacterToken {
    override def char: Char = ','
  }

  case class ColonToken(pos: Int) extends SingleCharacterToken {
    override def char: Char = ':'
  }

  case class SemicolonToken(pos: Int) extends SingleCharacterToken {
    override def char: Char = ';'
  }

  case class DotToken(pos: Int) extends SingleCharacterToken {
    override def char: Char = '.'
  }

  case class SeparatorTokenOld(separator: Separator, pos: Int) extends SingleCharacterToken {
    override def char: Char = separator.char
  }

  val singleCharacterTokenGenerators: Map[Char, Int => SingleCharacterToken] = Map(
    '(' -> OpenParenToken,
    '{' -> OpenBraceToken,
    ')' -> CloseParenToken,
    '}' -> CloseBraceToken,
    ',' -> CommaToken,
    ':' -> ColonToken,
    ';' -> SemicolonToken,
    '.' -> DotToken
  )

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

  case class MemberSelectToken(term: TermToken, dot: DotToken, member: IdentifierToken)
    extends CallableToken with CompositeToken {
    override def children: Seq[Token] = Seq(term, dot, member)
  }

  sealed trait TupleToken extends TermToken

  case class UnitToken(open: OpenParenToken, close: CloseParenToken) extends TupleToken with CompositeToken {
    override def children: Seq[Token] = Seq(open, close)
  }

  case class OneTupleToken(open: OpenParenToken, term: TermToken, close: CloseParenToken)
    extends TupleToken with CompositeToken {
    override def children: Seq[Token] = Seq(open, term, close)
  }

  case class MultiTupleUnfinishedToken(open: OpenParenToken, args: Seq[(TermToken, CommaToken)])
    extends CompositeToken {
    override def children: Seq[Token] = open +: args.flatMap { case (term, comma) => Seq(term, comma) }

    def extendBy(term: TermToken, comma: CommaToken): MultiTupleUnfinishedToken =
      MultiTupleUnfinishedToken(open, args :+ (term, comma))

    def closeBy(term: TermToken, close: CloseParenToken): MultiTupleToken =
      MultiTupleToken(open, args, term, close)
  }

  object MultiTupleUnfinishedToken {
    def apply(open: OpenParenToken, term: TermToken, comma: CommaToken): MultiTupleUnfinishedToken =
      MultiTupleUnfinishedToken(open, Seq((term, comma)))
  }

  case class MultiTupleToken(open: OpenParenToken, argsFirst: Seq[(TermToken, CommaToken)], argLast: TermToken,
                             close: CloseParenToken) extends TupleToken with CompositeToken {
    override def children: Seq[Token] =
      open +: argsFirst.flatMap { case (term, comma) => Seq(term, comma) } :+ argLast :+ close
  }

  case class CallToken(callable: CallableToken, tuple: TupleToken)
    extends TermToken with CompositeToken {
    override def children: Seq[Token] = Seq(callable, tuple)
  }

}