package chowser.interpreter

import chowser.expressions.Expression
import chowser.interpreter.tokenize.Token.ExpressionToken
import chowser.interpreter.tokenize.{ScanState, Scanner}
import chowser.interpreter.treemaker.{ChowserReduceRules, Reducer}

object ChowserInterpreter {

  def interpretString(string: String): Either[String, Expression] = {
    if(string == "exit()") {
      Right(Expression.Exit)
    } else {
      var state = ScanState(string)
      val scanner = Scanner.chowserScanner
      var count = 0
      while(!state.isFinal) {
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
      if(reduceResult.errors.nonEmpty) {
        Left("Could not parse\n" + reduceResult.errors.mkString("\n"))
      } else {
        if(reduceResult.tokens.size != 1) {
          if(reduceResult.tokens.isEmpty) {
            Left("Found no parsable tokens.")
          } else {
            Left("Superflous tokens " + reduceResult.tokens.tail.map(_.string).mkString(""))
          }
        } else {
          reduceResult.tokens.head match {
            case expressionToken: ExpressionToken =>
              val expression = expressionToken.expression
              Right(expression)
            case _ =>
              Left("Does not evaluate to an expression.")
          }
        }
      }
    }
  }

}
