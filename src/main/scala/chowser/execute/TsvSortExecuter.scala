package chowser.execute

import chowser.cmd.TsvSortCommand
import chowser.tsv.TsvReader

object TsvSortExecuter extends ChowserExecuter[TsvSortCommand] {

  def execute(command: TsvSortCommand): Result = {
    import command.{colName, inFile, outFile}
    ExecutionUtils.sortRowsByCol(inFile, outFile, TsvReader.forSimpleHeaderLine(_), colName)
    Result(command, success = true)
  }

  case class Result(command: TsvSortCommand, success: Boolean) extends ChowserExecuter.Result[TsvSortCommand]

}
