package chowser.parser.treemaker

import chowser.parser.tokenize.Token
import chowser.parser.tokenize.Token.{BinaryOpToken, OperatorToken, TermToken, UnaryOpToken, WhiteSpace}
import chowser.parser.treemaker.Reducer.State.{LNil, LSeq, Lhs}
import chowser.parser.treemaker.Reducer.{Rule, State}

object ChowserReduceRules {

  val skipWhiteSpace: Rule = {
    case State(LSeq(lTail, _: WhiteSpace), _) => Right(lTail)
  }

  val unaryOp: Rule = {
    case State(LSeq(LSeq(lTail, op: OperatorToken), term: TermToken), _)
      if !lTail.hasHeadWith(_.isInstanceOf[TermToken]) &&
        op.operator.canBeUnaryPrefix =>
      Right(LSeq(lTail, UnaryOpToken(op, term)))
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

  val all: Rule = skipWhiteSpace orElse unaryOp orElse binaryOp

}
