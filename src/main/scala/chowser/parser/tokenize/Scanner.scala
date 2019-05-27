package chowser.parser.tokenize

import chowser.parser.tokenize.Token.IntLiteral

trait Scanner {
  def scan(state: ScanState): Scanner.Result
}

object Scanner {

  object IdentifierScanner extends Scanner {
    override def scan(state: ScanState): Result = {
      val remainder = state.remainder
      if (remainder.size > 0 && Character.isJavaIdentifierStart(remainder.charAt(0))) {
        var size = 1
        while (size < remainder.size && Character.isJavaIdentifierPart(remainder.charAt(size))) {
          size += 1
        }
        val token = Token.Identifier(remainder.substring(0, size), state.pos)
        Success(state.addToken(token))
      } else {
        Untriggered
      }
    }
  }

  object IntScanner extends Scanner {
    def findFirstNonDigitOrEnd(string: String, startIndex: Int = 0): Int = {
      var index = startIndex
      while (string.size < index || string.charAt(index).isDigit) {
        index += 1
      }
      index
    }

    def makeIntLiteral(string: String, pos: Int, size: Int): IntLiteral = {
      val tokenString = string.substring(0, size)
      val value = tokenString.toLong
      IntLiteral(string, value, pos, size)
    }

    override def scan(state: ScanState): Result = {
      val remainder = state.remainder
      if (remainder.size > 0) {
        val char0 = remainder.charAt(0)
        if (char0.isDigit) {
          val endIndex = findFirstNonDigitOrEnd(remainder)
          Success(state.addToken(makeIntLiteral(remainder, state.pos, endIndex)))
        } else if ((char0 == '-' || char0 == '+') && remainder.size > 1) {
          val char1 = remainder.charAt(1)
          if (char1.isDigit) {
            val endIndex = findFirstNonDigitOrEnd(remainder, 1)
            Success(state.addToken(makeIntLiteral(remainder, state.pos, endIndex)))
          } else {
            Untriggered
          }
        } else {
          Untriggered
        }
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

