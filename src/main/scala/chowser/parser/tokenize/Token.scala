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

  case class MemberSelectToken(term: TermToken, dot: SeparatorToken, member: IdentifierToken)
    extends CallableToken with CompositeToken {
    override def children: Seq[Token] = Seq(term, dot, member)
  }

  sealed trait ArgsToken extends Token {
  }

  object ArgsToken {
    def forSingleArg(term: TermToken): NonZeroArgsToken = NonZeroArgsToken(term, Seq.empty)
  }

  case class ZeroArgsToken(pos: Int) extends ArgsToken {
    override def string: String = ""

    override def size: Int = 0
  }

  case class NonZeroArgsToken(head: TermToken, tail: Seq[(SeparatorToken, TermToken)])
    extends ArgsToken with CompositeToken {
    override def children: Seq[Token] = head +: tail.flatMap { case (separator, term) => Seq(separator, term) }

    def plus(separator: SeparatorToken, term: TermToken): NonZeroArgsToken =
      NonZeroArgsToken(head, tail :+ (separator, term))
  }

  case class CallPartialCloseableToken(callable: CallableToken, open: OpenBracketToken, args: ArgsToken)
    extends CompositeToken {
    override def children: Seq[Token] = Seq(callable, open, args)

    def closedWith(close: CloseBracketToken): CallToken = CallToken(callable, open, args, close)
  }

  object CallPartialCloseableToken {
    def apply(callable: CallableToken, open: OpenBracketToken): CallPartialCloseableToken =
      CallPartialCloseableToken(callable, open, ZeroArgsToken(open.pos + 1))
  }

  case class CallPartialTrailingCommaToken(callable: CallableToken, open: OpenBracketToken, args: ArgsToken,
                                           comma: SeparatorToken) extends CompositeToken {
    override def children: Seq[Token] = Seq(callable, open, args, comma)
  }

  case class CallToken(callable: CallableToken, open: OpenBracketToken, args: ArgsToken, close: CloseBracketToken)
    extends TermToken with CompositeToken {
    override def children: Seq[Token] = Seq(callable, open, args, close)
  }

}