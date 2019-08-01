package chowser.execute

import chowser.cmd.TsvRangeCommand
import chowser.filter.{RowFilters, StringFilters}
import chowser.tsv.{BasicTsvReader, TsvUtils}

object TsvRangeExecuter extends ChowserExecuter[TsvRangeCommand] {

  def execute(command: TsvRangeCommand): Result = {
    import command.{inFile, outFile, colName, filter}
    val rowFilter = RowFilters.ForCol(colName, StringFilters.parsesAsDoubleAndFilter(filter))
    TsvUtils.filterRows(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_), rowFilter)
    Result(command, success = true)
  }

  case class Result(command: TsvRangeCommand, success: Boolean) extends ChowserExecuter.Result[TsvRangeCommand]

}
