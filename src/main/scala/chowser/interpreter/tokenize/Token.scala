package chowser.interpreter.tokenize

import chowser.expressions.Expression.{ArgExpression, BinaryOpExpression, FloatLiteral, IdentifierExpression, IntLiteral, Literal, StringLiteral, TupleExpression, UnaryOpExpression, UnitLiteral}
import chowser.expressions.{Expression, Identifier, Operator}
import chowser.util.NumberParser

sealed trait Token {
  def string: String

  def pos: Int

  def size: Int
}

object Token {

  sealed trait TermToken extends Token

  case class WhiteSpaceToken(string: String, pos: Int, size: Int) extends Token

  sealed trait CallableToken extends TermToken

  def argumentOpt(string: String): Option[ArgExpression] = {
    if (string == "_") {
      Some(ArgExpression.createNewSliding())
    } else {
      if (string.startsWith("_") && string.endsWith("_") && string.size > 2) {
        val middle = string.substring(1, string.size - 1)
        if (NumberParser.UnsignedIntParser.isValid(middle)) {
          val pos = NumberParser.UnsignedIntParser.parse(middle)
          Some(ArgExpression.createNewPinned(pos))
        } else {
          None
        }
      } else {
        None
      }
    }
  }


  trait IdentifierToken extends CallableToken with ExpressionToken {
    def identifier: Identifier

    override def expression: Expression = {
      argumentOpt(string) match {
        case Some(argExpression) => argExpression
        case None => IdentifierExpression(Identifier(None, string))
      }
    }
  }

  case class LocalIdentifierToken(string: String, pos: Int, size: Int) extends IdentifierToken {
    override def identifier: Identifier = Identifier(None, string)
  }

  case class NamespacedIdentifier(namespace: IdentifierToken, dot: DotToken,
                                  localIdentifier: LocalIdentifierToken) extends IdentifierToken with CompositeToken {
    override def identifier: Identifier = Identifier(Some(namespace.identifier), localIdentifier.string)

    override def children: Seq[Token] = Seq(namespace, dot, localIdentifier)
  }

  object IdentifierToken {
    def apply(string: String, pos: Int): IdentifierToken = LocalIdentifierToken(string, pos, string.size)
  }

  case class OperatorToken(operator: Operator, pos: Int) extends Token {
    override def string: String = operator.string

    override def size: Int = string.size

    def precedence: Int = operator.precedence
  }

  sealed trait ExpressionToken extends TermToken {
    def expression: Expression
  }

  sealed trait LiteralToken extends ExpressionToken {
    def literal: Literal

    override def expression: Expression = literal
  }

  case class IntLiteralToken(string: String, literal: IntLiteral, pos: Int, size: Int)
    extends LiteralToken

  case class FloatLiteralToken(string: String, literal: FloatLiteral, pos: Int, size: Int)
    extends LiteralToken

  case class StringLiteralToken(string: String, literal: StringLiteral, pos: Int, size: Int)
    extends LiteralToken

  sealed trait TupleMakingToken extends Token {
    def args: Seq[ExpressionToken]

    def tupleMaker: TupleMakerToken
  }

  sealed trait UnfinishedTupleMakingToken extends TupleMakingToken {
    def prependArg(arg: ExpressionToken): TupleMakingToken = {
      val argsNew = arg +: args
      if (argsNew.size == tupleMaker.wantsToAbsorb) {
        TupleMakingExpressionToken(argsNew, tupleMaker)
      } else {
        PartialTupleMakingToken(argsNew, tupleMaker)
      }
    }

    def wantsToAbsorb: Int
  }

  case class TupleMakerToken(string: String, pos: Int, size: Int) extends UnfinishedTupleMakingToken {
    override def args: Seq[ExpressionToken] = Seq.empty

    override def tupleMaker: TupleMakerToken = this

    override def wantsToAbsorb: Int = size - 2
  }

  case class PartialTupleMakingToken(args: Seq[ExpressionToken], tupleMaker: TupleMakerToken)
    extends CompositeToken with UnfinishedTupleMakingToken {
    override def children: Seq[Token] = args :+ tupleMaker

    override def wantsToAbsorb: Int = tupleMaker.wantsToAbsorb - args.size
  }

  case class TupleMakingExpressionToken(args: Seq[ExpressionToken], tupleMaker: TupleMakerToken)
    extends TupleMakingToken with CompositeToken with ExpressionToken {
    override def children: Seq[Token] = args :+ tupleMaker

    override def expression: Expression = TupleExpression(args.map(_.expression))
  }

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

  case class HashToken(pos: Int) extends SingleCharacterToken {
    override def char: Char = '#'
  }

  val singleCharacterTokenGenerators: Map[Char, Int => SingleCharacterToken] = Map(
    '(' -> OpenParenToken,
    '{' -> OpenBraceToken,
    ')' -> CloseParenToken,
    '}' -> CloseBraceToken,
    ',' -> CommaToken,
    ':' -> ColonToken,
    ';' -> SemicolonToken,
    '.' -> DotToken,
    '#' -> HashToken
  )

  trait CompositeToken extends Token {
    def children: Seq[Token]

    override def string: String = children.map(_.string).mkString("")

    override def size: Int = children.map(_.size).sum

    override def pos: Int = children.head.pos
  }

  case class UnaryOpToken(op: OperatorToken, arg: ExpressionToken) extends ExpressionToken with CompositeToken {
    override def children: Seq[Token] = Seq(op, arg)

    override def expression: UnaryOpExpression = UnaryOpExpression(op.operator, arg.expression)
  }

  case class BinaryOpToken(lhs: ExpressionToken, op: OperatorToken, rhs: ExpressionToken)
    extends ExpressionToken with CompositeToken {
    override def children: Seq[Token] = Seq(lhs, op, rhs)

    override def expression: BinaryOpExpression = BinaryOpExpression(op.operator, lhs.expression, rhs.expression)
  }

  case class BracketedTermToken(open: OpenBracketToken, term: ExpressionToken, close: CloseBracketToken)
    extends ExpressionToken with CompositeToken {
    override def children: Seq[Token] = Seq(open, term, close)

    override def expression: Expression = term.expression
  }

  case class MemberSelectToken(term: ExpressionToken, dot: DotToken, member: IdentifierToken)
    extends CallableToken with CompositeToken {
    override def children: Seq[Token] = Seq(term, dot, member)
  }

  sealed trait TupleToken extends ExpressionToken

  case class UnitToken(open: OpenParenToken, close: CloseParenToken) extends TupleToken with CompositeToken {
    override def children: Seq[Token] = Seq(open, close)

    override def expression: Expression = UnitLiteral
  }

  case class OneTupleToken(open: OpenParenToken, term: ExpressionToken, close: CloseParenToken)
    extends TupleToken with CompositeToken {
    override def children: Seq[Token] = Seq(open, term, close)

    override def expression: Expression = term.expression
  }

  case class MultiTupleUnfinishedToken(open: OpenParenToken, args: Seq[(ExpressionToken, CommaToken)])
    extends CompositeToken {
    override def children: Seq[Token] = open +: args.flatMap { case (term, comma) => Seq(term, comma) }

    def extendBy(term: ExpressionToken, comma: CommaToken): MultiTupleUnfinishedToken =
      MultiTupleUnfinishedToken(open, args :+ (term, comma))

    def closeBy(term: ExpressionToken, close: CloseParenToken): MultiTupleToken =
      MultiTupleToken(open, args, term, close)
  }

  object MultiTupleUnfinishedToken {
    def apply(open: OpenParenToken, term: ExpressionToken, comma: CommaToken): MultiTupleUnfinishedToken =
      MultiTupleUnfinishedToken(open, Seq((term, comma)))
  }

  case class MultiTupleToken(open: OpenParenToken, argsFirst: Seq[(ExpressionToken, CommaToken)],
                             argLast: ExpressionToken, close: CloseParenToken) extends TupleToken with CompositeToken {
    override def children: Seq[Token] =
      open +: argsFirst.flatMap { case (term, comma) => Seq(term, comma) } :+ argLast :+ close

    def elementTokens: Seq[ExpressionToken] = argsFirst.map(_._1) :+ argLast

    override def expression: Expression = TupleExpression(elementTokens.map(_.expression))
  }

  case class CallTokenOld(term: TermToken, callable: CallableToken) extends TermToken with CompositeToken {
    override def children: Seq[Token] = Seq(term, callable)
  }

  case class CallToken(args: Seq[ExpressionToken], identifier: IdentifierToken)
    extends ExpressionToken with CompositeToken {
    override def expression: Expression = ???

    override def children: Seq[Token] = ???
  }

  case class ConcatToken(lhs: ExpressionToken, hash: HashToken, rhs: ExpressionToken)
    extends ExpressionToken with CompositeToken {
    private def asTupleArgs(expression: Expression): Seq[Expression] = {
      expression match {
        case tupleExpression: TupleExpression => tupleExpression.elements
        case _ => Seq(expression)
      }
    }
    override def expression: Expression = {
      TupleExpression(asTupleArgs(lhs.expression) ++ asTupleArgs(rhs.expression))
    }

    override def children: Seq[Token] = Seq(lhs, hash, rhs)
  }

}