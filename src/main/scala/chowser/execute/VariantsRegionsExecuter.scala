package chowser.execute

import chowser.cmd.VariantsRegionsCommand

object VariantsRegionsExecuter {

  def execute(command: VariantsRegionsCommand): Result = {
    
    println("Not implemented yet!") //  TODO
    Result(command, success = false)
  }

  case class Result(command: VariantsRegionsCommand, success: Boolean) extends ChowserExecuter.Result

}
