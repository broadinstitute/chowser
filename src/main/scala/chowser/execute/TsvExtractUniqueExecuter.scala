package chowser.execute

import chowser.cmd.TsvExtractUniqueCommand
import chowser.tsv.TsvReader

object TsvExtractUniqueExecuter extends ChowserExecuter[TsvExtractUniqueCommand] {

  def execute(command: TsvExtractUniqueCommand): Result = {
    import command.{colName, inFile, outFile}
    ExecutionUtils.extractUniqueValues(inFile, outFile, TsvReader.forSimpleHeaderLine(_), colName)
    Result(command, success = true)
  }

  case class Result(command: TsvExtractUniqueCommand, success: Boolean)
    extends ChowserExecuter.Result[TsvExtractUniqueCommand]

}
