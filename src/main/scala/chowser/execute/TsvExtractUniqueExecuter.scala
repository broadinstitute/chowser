package chowser.execute

import chowser.cmd.TsvExtractUniqueCommand
import chowser.tsv.{BasicTsvReader, TsvUtils}

object TsvExtractUniqueExecuter extends ChowserExecuter[TsvExtractUniqueCommand] {

  def execute(command: TsvExtractUniqueCommand): Result = {
    import command.{colName, inFile, outFile}
    TsvUtils.extractUniqueValues(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_), colName)
    Result(command, success = true)
  }

  case class Result(command: TsvExtractUniqueCommand, success: Boolean)
    extends ChowserExecuter.Result[TsvExtractUniqueCommand]

}
