package chowser.execute

import chowser.cmd.LiftoverTsvCommand

object LiftoverTsvExecuter extends ChowserExecuter[LiftoverTsvCommand] {

  def execute(command: LiftoverTsvCommand): Result = {
    ???
    Result(command, success = true)
  }

  case class Result(command: LiftoverTsvCommand, success: Boolean) extends ChowserExecuter.Result[LiftoverTsvCommand]

}
