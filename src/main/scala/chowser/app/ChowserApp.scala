package chowser.app

import chowser.execute.ChowserExecuter

object ChowserApp {

  def main(args: Array[String]): Unit = {
    val conf = new ChowserConf(args)
    println(conf.summary)
    val commandEither = conf.toChowserCommand
    commandEither match {
      case Left(string) => println(string)
      case Right(command) =>
        println(command)
        val result = ChowserExecuter.execute(command)
        println(result)
    }
    println("Done!")
  }

}
