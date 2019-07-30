package chowser.execute

import chowser.cmd.ShellCommand
import chowser.expressions.defs.predef.ChowserPredefDefs
import chowser.expressions.{ChowserRuntime, Context}
import chowser.interpreter.ChowserInterpreter

object ShellExecuter extends ChowserExecuter[ShellCommand.type] {
  override def execute(command: ShellCommand.type): Result = execute()

  def execute(): Result = {
    println("Welcome to ChowserShell!")
    val context = Context.predef
    val runtime = new ChowserRuntime
    val symbolTable = ChowserPredefDefs.symbolTable
    while(!runtime.exitIsRequested) {
      print("chowser> ")
      val input = Console.in.readLine()
      ChowserInterpreter.interpretString(input) match {
        case Left(message) => println(message)
        case Right(expression) =>
          expression.evaluate(runtime, symbolTable) match {
            case Right(value) => println(value.asStringWithType)
            case Left(message) => println(message)
          }
      }
    }
    Result(true)
  }

  case class Result(success: Boolean) extends ChowserExecuter.Result[ShellCommand.type] {
    override def command: ShellCommand.type = ShellCommand
  }


}
