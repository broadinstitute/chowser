package chowser.parser.treemaker

import chowser.parser.tokenize.Token
import chowser.parser.tokenize.Token._
import chowser.parser.treemaker.Reducer.State.{LSeq, RSeq}
import chowser.parser.treemaker.Reducer.{Rule, State}

object ChowserReduceRules {

  val skipWhiteSpace: Rule = {
    case State(LSeq(lTail, _: WhiteSpaceToken), _) => Right(lTail)
  }

  val unaryOp: Rule = {
    case State(LSeq(LSeq(lTail, op: OperatorToken), term: TermToken), _)
      if !lTail.hasHeadWith(_.isInstanceOf[TermToken]) =>
      if (op.operator.canBeUnaryPrefix) {
        Right(LSeq(lTail, UnaryOpToken(op, term)))
      } else {
        Left(s"'${op.string}' is not a valid unary prefix operator.")
      }
  }

  def isOpHigherThan(precedence: Int): Token => Boolean = {
    case op: OperatorToken => op.precedence > precedence
    case _ => false
  }

  val binaryOp: Rule = {
    case State(LSeq(LSeq(LSeq(lTail, term1: TermToken), op: OperatorToken), term2: TermToken), rhs)
      if !rhs.hasHeadWith(isOpHigherThan(op.precedence)) =>
      Right(LSeq(lTail, BinaryOpToken(term1, op, term2)))
  }

  val bracketedTerm: Rule = {
    case State(LSeq(LSeq(LSeq(lTail, open: OpenBracketToken), term: TermToken), close: CloseBracketToken), _) =>
      if (open.openBracket.closer == close.closeBracket) {
        Right(LSeq(lTail, BracketedTermToken(open, term, close)))
      } else {
        Left(s"'${open.string}' cannot be closed with '${close.string}'.")
      }
  }

  val memberSelection: Rule = {
    case State(LSeq(LSeq(LSeq(lTail, term: TermToken), dot: SeparatorToken), identifier: IdentifierToken), _)
      if dot.separator.char == '.' =>
      Right(LSeq(lTail, MemberSelectToken(term, dot, identifier)))
  }

  val startCall: Rule = {
    case State(LSeq(LSeq(lTail, callable: CallableToken), open: OpenBracketToken), _)
    if open.char == '(' =>
      Right(LSeq(lTail, CallPartialCloseableToken(callable, open)))
  }

  def argIsComplete(token: Token): Boolean = ???

  val callPlusFirstArg: Rule = {
    case State(LSeq(LSeq(lTail, CallPartialCloseableToken(callable, open, ZeroArgsToken(_))), term: TermToken), rhs)
      if !rhs.hasHeadWith(_.isInstanceOf[OperatorToken]) =>
      Right(LSeq(lTail, CallPartialCloseableToken(callable, open, ArgsToken.forSingleArg(term))))
  }

  val callAddComma: Rule = {
    case State(LSeq(LSeq(lTail, CallPartialCloseableToken(callable, open, args)), separator: SeparatorToken), _)
    if separator.char == ',' =>
      Right(LSeq(lTail, CallPartialTrailingCommaToken(callable, open, args, separator)))
  }

  val callAdditionalArg: Rule = {
    case State(LSeq(LSeq(lTail,
    CallPartialTrailingCommaToken(callable, open, args: NonZeroArgsToken, comma)), term: TermToken), rhs)
      if !rhs.hasHeadWith(_.isInstanceOf[OperatorToken]) =>
      Right(LSeq(lTail, CallPartialCloseableToken(callable, open, args.plus(comma, term))))
  }

  val callClose: Rule = {
    case State(LSeq(LSeq(lTail, closable: CallPartialCloseableToken), close: CloseBracketToken), _)
      if close.char == ')' =>
      Right(LSeq(lTail, closable.closedWith(close)))
  }

  val all: Rule =
    skipWhiteSpace orElse unaryOp orElse binaryOp orElse bracketedTerm orElse memberSelection orElse
      startCall orElse callPlusFirstArg orElse callAddComma orElse callAdditionalArg orElse callClose

}
