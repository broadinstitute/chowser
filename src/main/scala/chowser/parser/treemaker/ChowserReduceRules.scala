package chowser.parser.treemaker

import chowser.parser.tokenize.Token.{OperatorToken, TermToken, UnaryOpToken, WhiteSpace}
import chowser.parser.treemaker.Reducer.State.{LNil, LSeq, Lhs}
import chowser.parser.treemaker.Reducer.{Rule, State}

object ChowserReduceRules {

  val skipWhiteSpace: Rule = {
    case State(LSeq(lTail, _: WhiteSpace), _) => Right(lTail)
  }

  val unaryOp: Rule = {
    case State(LSeq(LSeq(lTail, op: OperatorToken),term: TermToken), _)
      if lTail.hasNoTermHead && op.operator.canBeUnaryPrefix => Right(LSeq(lTail, UnaryOpToken(op, term)))
  }

  val all: Rule = skipWhiteSpace orElse unaryOp

}
