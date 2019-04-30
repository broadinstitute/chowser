package chowser.execute

import chowser.cmd.VariantsCanonicalizeVcfCommand

object VariantsCanonicalizeVcfExecuter extends ChowserExecuter[VariantsCanonicalizeVcfCommand] {

  def execute(command: VariantsCanonicalizeVcfCommand): Result = {
    import command._
    ???
    Result(command, success = true)
  }

  case class Result(command: VariantsCanonicalizeVcfCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsCanonicalizeVcfCommand]

}
