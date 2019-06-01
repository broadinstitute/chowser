package chowser.parser.treemaker

import chowser.parser.tokenize.Token

object Reducer {

  def reduce(tokens: Seq[Token], rules: Seq[Rule]): Result = {
    var state: State = State(tokens)
    var errors: Seq[String] = Seq.empty
    var keepGoing: Boolean = true
    while(keepGoing) {
      var rulesIter: Iterator[Rule] = rules.iterator
      while(rulesIter.hasNext) {
        val rule = rulesIter.next()
        rule.reduce(state) match {
          case Rule.Untriggered => ()
          case Rule.Error(message) => errors :+= message
          case Rule.Success(stateNew) =>
            state = stateNew
            rulesIter = rules.iterator
        }
      }
      if(state.isExhausted) {
        keepGoing = false
      } else {
        state = state.shift
      }
    }
    Result(state.lhs, errors)
  }

  trait Reducible[+R <: Reducible[R]] {
    def lhs: Seq[Token]
    def rhs: Seq[Token]
    def reduce(nConsume: Int, replacements: Seq[Token]): R
  }

  case class State(lhs: Seq[Token], rhs: Seq[Token]) extends Reducible[State] {
    def isExhausted: Boolean = rhs.isEmpty
    def reduce(nConsume: Int, replacements: Seq[Token]): State = copy(lhs = lhs.dropRight(nConsume) ++ replacements)
    def shift: State = State(lhs :+ rhs.head, rhs.tail)
  }

  object State {
    def apply(tokens: Seq[Token]): State = State(Seq.empty, tokens)
  }

  trait Rule {
    def reduce[R <: Reducible[R]](reducible: R): Rule.Result[R]
  }
  object Rule {
    sealed trait Result[+R <: Reducible[R]]
    object Untriggered extends Result[Nothing]
    case class Error(message: String) extends Result[Nothing]
    case class Success[R <: Reducible[R]](reducible: R) extends Result[R]
  }

  case class Result(tokens: Seq[Token], errors: Seq[String])


}
