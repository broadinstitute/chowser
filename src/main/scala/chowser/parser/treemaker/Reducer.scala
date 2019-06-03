package chowser.parser.treemaker

import chowser.parser.tokenize.Token
import chowser.parser.tokenize.Token.TermToken
import chowser.parser.treemaker.Reducer.State.{LSeq, Lhs, RNil, RSeq, Rhs}

object Reducer {

  def reduce(tokens: Seq[Token], rule: Rule): Result = {
    var state: State = State(tokens)
    var errors: Seq[String] = Seq.empty
    var keepGoing: Boolean = true
    while (keepGoing) {
      rule.lift(state) match {
        case Some(Left(error)) =>
          keepGoing = false
          errors :+= error
        case Some(Right(reduction)) =>
          state = state.reduce(reduction)
        case None =>
          state.shift match {
            case Some(stateNew) => state = stateNew
            case None => keepGoing = false
          }
      }
      println(state.asString)
    }
    Result(state.lhs.tokens, errors)
  }

  case class State(lhs: Lhs, rhs: Rhs) extends State.HasTokens {
    override def asString: String = lhs.asString + "||" + rhs.asString

    def reduce(lhsNew: Lhs): State = copy(lhs = lhsNew)

    def isExhausted: Boolean = rhs.isEmpty

    def shift: Option[State] = {
      rhs match {
        case RNil => None
        case RSeq(head, tail) => Some(State(LSeq(lhs, head), tail))
      }
    }

    override def isEmpty: Boolean = lhs.isEmpty && rhs.isEmpty

    override def tokens: Seq[Token] = lhs.tokens ++ rhs.tokens
  }

  object State {
    def apply(tokens: Seq[Token]): State = State(LNil, Rhs(tokens))

    sealed trait HasTokens {
      def isEmpty: Boolean

      def tokens: Seq[Token]

      def asString: String = tokens.map(_.string).mkString("|")
    }

    sealed trait TokenList extends HasTokens {
      def hasHeadWith(predicate: Token => Boolean): Boolean
    }

    sealed trait EmptyTokenList extends TokenList {
      override final def hasHeadWith(predicate: Token => Boolean): Boolean = false
    }

    sealed trait NonEmptyTokenList extends TokenList {
      def head: Token

      override final def hasHeadWith(predicate: Token => Boolean): Boolean = predicate(head)
    }

    sealed trait Lhs extends TokenList {
    }

    object LNil extends Lhs with EmptyTokenList {
      override def isEmpty: Boolean = true

      override def tokens: Seq[Token] = Seq.empty

    }

    case class LSeq(tail: Lhs, head: Token) extends Lhs with NonEmptyTokenList {
      override def isEmpty: Boolean = false

      override def tokens: Seq[Token] = tail.tokens :+ head
    }

    sealed trait Rhs extends TokenList {
    }

    object RNil extends Rhs with EmptyTokenList {
      override def isEmpty: Boolean = true

      override def tokens: Seq[Token] = Seq.empty
    }

    case class RSeq(head: Token, tail: Rhs) extends Rhs with NonEmptyTokenList {
      override def isEmpty: Boolean = false

      override def tokens: Seq[Token] = head +: tail.tokens
    }

    object Rhs {
      def apply(tokens: Seq[Token]): Rhs = {
        tokens.headOption match {
          case Some(head) => RSeq(head, Rhs(tokens.tail))
          case None => RNil
        }
      }
    }

  }

  type Rule = PartialFunction[State, Either[String, Lhs]]

  case class Result(tokens: Seq[Token], errors: Seq[String])

}
