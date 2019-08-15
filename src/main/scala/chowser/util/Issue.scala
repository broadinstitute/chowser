package chowser.util

sealed trait Issue {

}

case class SimpleIssue(message: String) extends Issue

object Issue {
  def forMessage(message: String): Issue = SimpleIssue(message)
}