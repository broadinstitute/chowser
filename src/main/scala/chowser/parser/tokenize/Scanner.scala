package chowser.parser.tokenize

trait Scanner {
  def scan(state: ScanState): Scanner.Result
}

object Scanner {
  sealed trait Result
  object Untriggered extends Result
  case class TriggeredButFailed(message: String) extends Result
  case class Success(state: ScanState)
}

