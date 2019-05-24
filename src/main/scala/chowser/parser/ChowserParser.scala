package chowser.parser

import chowser.expressions.{Exit, Expression}

object ChowserParser {

  def parseString(string: String): Either[String, Expression] = {
    if(string == "exit()") {
      Right(Exit)
    } else {
      Left("Don't know how to parse that yet.")
    }
  }

}
