package chowser.execute

import chowser.cmd.ShellCommand
import chowser.expressions.{Context, Failure, Issue, Success}
import chowser.interpreter.ChowserInterpreter

object ShellExecuter extends ChowserExecuter[ShellCommand.type] {
  override def execute(command: ShellCommand.type): Result = execute()

  def execute(): Result = {
    println("Welcome to ChowserShell!")
    val context = new Context
    while(!context.exitIsRequested) {
      print("chowser> ")
      val input = Console.in.readLine()
      ChowserInterpreter.interpretString(input) match {
        case Left(message) => println(message)
        case Right(expression) =>
          expression.evaluate(context) match {
            case Success(value) => println(value.asStringWithType)
            case Failure(Issue(message)) => println(message)
          }
      }
    }
    Result(true)
  }

  case class Result(success: Boolean) extends ChowserExecuter.Result[ShellCommand.type] {
    override def command: ShellCommand.type = ShellCommand
  }


}
