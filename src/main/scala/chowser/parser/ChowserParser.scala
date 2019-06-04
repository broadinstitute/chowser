package chowser.parser

import chowser.expressions.Expression
import chowser.parser.tokenize.{ScanState, Scanner}
import chowser.parser.treemaker.{ChowserReduceRules, Reducer}

object ChowserParser {

  def parseString(string: String): Either[String, Expression] = {
    if(string == "exit()") {
      Right(Expression.Exit)
    } else {
      var state = ScanState(string)
      val scanner = Scanner.chowserScanner
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
      val reduceResult = Reducer.reduce(state.tokens, ChowserReduceRules.all)
      if(reduceResult.errors.nonEmpty) {
        println("Error(s) during reduction")
        println(reduceResult.errors.mkString("\n"))
      }
      println(reduceResult.tokens.map(_.string).mkString("|"))
      println(reduceResult)
      Left("Work in progress")
    }
  }

}
