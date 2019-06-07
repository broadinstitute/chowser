package chowser.expressions

import chowser.expressions.values.Value

sealed trait Result {

}

case class Issue(message: String)

case class Success(value: Value) extends Result

case class Failure(issue: Issue) extends Result

object Failure {
  def apply(message: String): Failure = Failure(Issue(message))
}


