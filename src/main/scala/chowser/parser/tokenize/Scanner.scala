package chowser.parser.tokenize

trait Scanner {
  def scan(state: ScanState): Scanner.Result
}

object Scanner {
  object IdentifierScanner extends Scanner {
    override def scan(state: ScanState): Result = {
      val remainder = state.remainder
      if(Character.isJavaIdentifierStart(remainder.charAt(0))) {
        var size = 1
        while(size < remainder.size && Character.isJavaIdentifierPart(remainder.charAt(size))) {
          size +=1
        }
        val token = Token.Identifier(remainder.substring(0, size), state.pos)
        Success(state.addToken(token))
      } else {
        Untriggered
      }
    }
  }
  sealed trait Result
  object Untriggered extends Result
  case class TriggeredButFailed(message: String) extends Result
  case class Success(state: ScanState) extends Result
}

