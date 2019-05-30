package chowser.parser.tokenize

import java.util.regex.Pattern

import chowser.parser.tokenize.Token.{IntLiteral, WhiteSpace}

trait Scanner {
  def scan(state: ScanState): Scanner.Result
}

object Scanner {

  case class CombinedScanner(scanners: Seq[Scanner]) extends Scanner {
    def pickBestResult(result1: Result, result2: Result): Result = {
      (result1, result2) match {
        case (Success(state1), Success(state2)) => if (state1.pos < state2.pos) result2 else result1
        case (Success(_), _) => result1
        case (_, Success(_)) => result2
        case (TriggeredButFailed(message1), TriggeredButFailed(message2)) =>
          TriggeredButFailed(message1 + "\n" + message2)
        case (TriggeredButFailed(_), _) => result1
        case (_, TriggeredButFailed(_)) => result2
        case _ => Untriggered
      }

    }

    override def scan(state: ScanState): Result = {
      scanners.map(_.scan(state)).fold(Untriggered)(pickBestResult)
    }
  }

  object CombinedScanner {
    def apply(scanner: Scanner, scanners: Scanner*): CombinedScanner = CombinedScanner(scanner +: scanners)
  }

  object WhiteSpaceScanner extends Scanner {
    override def scan(state: ScanState): Result = {
      val remainder = state.remainder
      var size = 0
      while (size < remainder.size && remainder.charAt(size).isWhitespace) {
        size += 1
      }
      if (size > 0) {
        val wsString = remainder.substring(0, size)
        Success(state.addToken(WhiteSpace(wsString, state.pos, size)))
      } else {
        Untriggered
      }
    }
  }

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

  object OperatorScanner extends Scanner {
    val operatorChars: Set[Char] = "+-=*/\\&%$@^|!~".toSet

    override def scan(state: ScanState): Result = {
      val remainder = state.remainder
      var size = 0
      while (size < remainder.size && operatorChars(remainder.charAt(size))) {
        size += 1
      }
      if (size > 0) {
        val operatorString = remainder.substring(0, size)
        Success(state.addToken(Token.Operator(operatorString, state.pos)))
      } else {
        Untriggered
      }
    }
  }

  object IntScanner extends Scanner {
    def findFirstNonDigitOrEnd(string: String, startIndex: Int = 0): Int = {
      var index = startIndex
      while (index < string.size && string.charAt(index).isDigit) {
        index += 1
      }
      index
    }

    def makeIntLiteral(string: String, pos: Int, size: Int): IntLiteral = {
      val tokenString = string.substring(0, size)
      val value = tokenString.toLong
      IntLiteral(tokenString, value, pos, size)
    }

    override def scan(state: ScanState): Result = {
      val remainder = state.remainder
      if (remainder.size > 0) {
        val char0 = remainder.charAt(0)
        if (char0.isDigit) {
          val endIndex = findFirstNonDigitOrEnd(remainder)
          Success(state.addToken(makeIntLiteral(remainder, state.pos, endIndex)))
        } else {
          Untriggered
        }
      } else {
        Untriggered
      }
    }
  }

  object FloatScanner extends Scanner {
    val regex: String = "[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"
    val pattern: Pattern = Pattern.compile(regex)

    def isFloatString(string: String): Boolean = pattern.matcher(string).matches()

    override def scan(state: ScanState): Result = {
      val remainder = state.remainder
      var size = 0
      var nFailures = 0
      val nFailuresMax = 2
      var floatString = ""
      while (size < remainder.size && nFailures < nFailuresMax) {
        size += 1
        val substring = state.remainder.substring(0, size)
        if (isFloatString(substring)) {
          floatString = substring
          nFailures = 0
        } else {
          nFailures += 1
        }
      }
      size = floatString.size
      if (floatString.nonEmpty) {
        val value = floatString.toDouble
        Success(state.addToken(Token.FloatLiteral(floatString, value, state.pos, size)))
      } else {
        Untriggered
      }
    }
  }

  object StringScanner extends Scanner {
    val escapes: Map[Char, Char] =
      Map('b' -> '\b', 'n' -> '\n', 't' -> '\t', 'r' -> '\r', 'f' -> '\f', '"' -> '"', '\\' -> '\\')

    override def scan(state: ScanState): Result = {
      ???
    }
  }

  sealed trait Result

  object Untriggered extends Result

  case class TriggeredButFailed(message: String) extends Result

  case class Success(state: ScanState) extends Result

}

