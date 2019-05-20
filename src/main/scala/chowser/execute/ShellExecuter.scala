package chowser.execute

import chowser.cmd.ShellCommand

object ShellExecuter extends ChowserExecuter[ShellCommand.type] {
  override def execute(command: ShellCommand.type): Result = execute()

  def execute(): Result = {
    var keepGoing = true
    while(keepGoing) {
      print("chowser> ")
      val input = Console.in.readLine()
      println(input)
      if(input == ":exit") {
        keepGoing = false
      }
    }
    println("Welcome to ChowserShell!")
    Result(true)
  }

  case class Result(success: Boolean) extends ChowserExecuter.Result[ShellCommand.type] {
    override def command: ShellCommand.type = ShellCommand
  }


}
