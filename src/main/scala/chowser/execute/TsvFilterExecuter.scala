package chowser.execute

import chowser.cmd.TsvFilterCommand
import chowser.filter.{RowFilters, StringFilters}
import chowser.tsv.TsvReader

object TsvFilterExecuter extends ChowserExecuter[TsvFilterCommand] {

  def execute(command: TsvFilterCommand): Result = {
    import command.{inFile, outFile, colName, filter}
    val rowFilter = RowFilters.ForCol(colName, StringFilters.parsesAsDoubleAndFilter(filter))
    ExecutionUtils.filterRows(inFile, outFile, TsvReader.forSimpleHeaderLine(_), rowFilter)
    Result(command, success = true)
  }

  case class Result(command: TsvFilterCommand, success: Boolean) extends ChowserExecuter.Result[TsvFilterCommand]

}
