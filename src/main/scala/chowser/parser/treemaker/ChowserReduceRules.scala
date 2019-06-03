package chowser.parser.treemaker

import chowser.parser.tokenize.Token
import chowser.parser.tokenize.Token._
import chowser.parser.treemaker.Reducer.State.LSeq
import chowser.parser.treemaker.Reducer.{Rule, State}

object ChowserReduceRules {

  val skipWhiteSpace: Rule = {
    case State(LSeq(lTail, _: WhiteSpace), _) => Right(lTail)
  }

  val unaryOp: Rule = {
    case State(LSeq(LSeq(lTail, op: OperatorToken), term: TermToken), _)
      if !lTail.hasHeadWith(_.isInstanceOf[TermToken]) =>
        if(op.operator.canBeUnaryPrefix) {
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
      if(open.openBracket.closer == close.closeBracket) {
        Right(LSeq(lTail, BracketedTermToken(open, term, close)))
      } else {
        Left(s"'${open.string}' cannot be closed with '${close.string}'.")
      }
  }

  val all: Rule = skipWhiteSpace orElse unaryOp orElse binaryOp orElse bracketedTerm

}
