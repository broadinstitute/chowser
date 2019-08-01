package chowser.execute

import chowser.cmd.TsvSortCommand
import chowser.tsv.{BasicTsvReader, TsvUtils}

object TsvSortExecuter extends ChowserExecuter[TsvSortCommand] {

  def execute(command: TsvSortCommand): Result = {
    import command.{colName, inFile, outFile}
    TsvUtils.sortRowsByCol(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_), colName)
    Result(command, success = true)
  }

  case class Result(command: TsvSortCommand, success: Boolean) extends ChowserExecuter.Result[TsvSortCommand]

}
