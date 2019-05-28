package chowser.parser

import chowser.expressions.{Exit, Expression}
import chowser.parser.tokenize.ScanState
import chowser.parser.tokenize.Scanner._

object ChowserParser {

  def parseString(string: String): Either[String, Expression] = {
    if(string == "exit()") {
      Right(Exit)
    } else {
      var state = ScanState(string)
      val scanner = CombinedScanner(WhiteSpaceScanner, IdentifierScanner, IntScanner, FloatScanner)
      var count = 0
      while(!state.isFinal) {
        println(state.asString)
        state = state.scanned(scanner)
        count += 1
      }
      println(state.asString)
      if(state.isComplete) {
        println("Tokenization complete.")
      } else if(state.hasFailed) {
        println("Tokenization has failed.")
      }
      Left(state.errorOption.getOrElse("No error reported."))
    }
  }

}
