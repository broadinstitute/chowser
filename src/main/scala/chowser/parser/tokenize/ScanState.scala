package chowser.parser.tokenize

import chowser.parser.tokenize.Scanner.{Success, TriggeredButFailed, Untriggered}

case class ScanState(tokens: Seq[Token], pos: Int, string: String, remainder: String, errorOption: Option[String]) {

  def addToken(token: Token): ScanState =
    ScanState(tokens :+ token, pos + token.size, string, remainder.substring(token.size), None)

  def withError(message: String): ScanState = copy(errorOption = Some(message))

  def isComplete: Boolean = remainder.isEmpty
  def hasFailed: Boolean = errorOption.nonEmpty
  def isFinal: Boolean = isComplete || hasFailed

  def asString: String = {
    val tokensString = tokens.map(_.string).mkString("|")
    val remainderString = if(remainder.isEmpty) {
      ""
    } else {
      "||" + remainder
    }
    val errorString = errorOption match {
      case Some(message) => "!!!" + message
      case None => ""
    }
    tokensString + remainderString + errorString
  }

  def scanned(scanner: Scanner): ScanState = {
    scanner.scan(this) match {
      case Success(state) => state
      case Untriggered => withError("Don't know how to scan this.")
      case TriggeredButFailed(message) => withError(message)
    }
  }

}

object ScanState {
  def apply(string: String): ScanState = ScanState(Seq.empty, 0, string, string, None)
}
