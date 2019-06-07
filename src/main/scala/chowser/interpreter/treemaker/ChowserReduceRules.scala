package chowser.interpreter.treemaker

import chowser.interpreter.tokenize.Token
import chowser.interpreter.tokenize.Token._
import chowser.interpreter.treemaker.Reducer.State.LSeq
import chowser.interpreter.treemaker.Reducer.{Rule, State}

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

  val memberSelection: Rule = {
    case State(LSeq(LSeq(LSeq(lTail, term: TermToken), dot: DotToken), identifier: IdentifierToken), _) =>
      Right(LSeq(lTail, MemberSelectToken(term, dot, identifier)))
  }

  val unit: Rule = {
    case State(LSeq(LSeq(lTail, open: OpenParenToken), close: CloseParenToken), _) =>
      Right(LSeq(lTail, UnitToken(open, close)))
  }

  val oneTuple: Rule = {
    case State(LSeq(LSeq(LSeq(lTail, open: OpenParenToken), term: TermToken), close: CloseParenToken), _) =>
      Right(LSeq(lTail, OneTupleToken(open, term, close)))
  }

  val multiTupleStart: Rule = {
    case State(LSeq(LSeq(LSeq(lTail, open: OpenParenToken), term: TermToken), comma: CommaToken), _) =>
      Right(LSeq(lTail, MultiTupleUnfinishedToken(open, term, comma)))
  }

  val multiTupleExtend: Rule = {
    case
      State(LSeq(LSeq(LSeq(lTail, multiTupleUnfinished: MultiTupleUnfinishedToken), term: TermToken),
      comma: CommaToken), _)
    =>
      Right(LSeq(lTail, multiTupleUnfinished.extendBy(term, comma)))
  }

  val multiTupleClose: Rule = {
    case
      State(LSeq(LSeq(LSeq(lTail, multiTupleUnfinished: MultiTupleUnfinishedToken), term: TermToken),
      close: CloseParenToken), _)
    =>
      Right(LSeq(lTail, multiTupleUnfinished.closeBy(term, close)))
  }

  val termInParens: Rule = {
    case State(LSeq(lTail, OneTupleToken(open, term, close)),  _) =>
      Right(LSeq(lTail, BracketedTermToken(open, term, close)))
  }

  val call: Rule = {
    case State(LSeq(LSeq(lTail, term: TermToken), callable: CallableToken), _) =>
      Right(LSeq(lTail, CallToken(term, callable)))
  }

  val all: Rule =
    skipWhiteSpace orElse unaryOp orElse binaryOp orElse memberSelection orElse unit orElse oneTuple orElse
      multiTupleStart orElse multiTupleExtend orElse multiTupleClose orElse termInParens orElse call

}
