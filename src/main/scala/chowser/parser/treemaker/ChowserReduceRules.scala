package chowser.parser.treemaker

import chowser.parser.tokenize.Token.WhiteSpace
import chowser.parser.treemaker.Reducer.State.LSeq
import chowser.parser.treemaker.Reducer.{Rule, State}

object ChowserReduceRules {

  val skipWhiteSpace: Rule = {
    case State(LSeq(lTail, _: WhiteSpace), _) => Right(lTail)
  }

}
