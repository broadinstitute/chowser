package chowser.execute

import chowser.cmd.TsvSortIdsCommand
import chowser.tsv.{BasicTsvReader, TsvUtils}

object TsvSortIdsExecuter extends ChowserExecuter[TsvSortIdsCommand] {

  def execute(command: TsvSortIdsCommand): Result = {
    import command.{colName, inFile, outFile}
    TsvUtils.sortRowsByIds(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_), colName)
    Result(command, success = true)
  }

  case class Result(command: TsvSortIdsCommand, success: Boolean) extends ChowserExecuter.Result[TsvSortIdsCommand]

}
