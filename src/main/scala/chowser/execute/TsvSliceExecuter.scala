package chowser.execute

import chowser.cmd.{TsvRangeCommand, TsvSliceCommand}
import chowser.filter.{RowFilters, StringFilters}
import chowser.tsv.{BasicTsvReader, TsvUtils}

object TsvSliceExecuter extends ChowserExecuter[TsvSliceCommand] {

  def execute(command: TsvSliceCommand): Result = {
    import command.{inFile, outFile, colName, filter}
    val rowFilter = RowFilters.ForCol(colName, filter)
    TsvUtils.filterRows(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_), rowFilter)
    Result(command, success = true)
  }

  case class Result(command: TsvSliceCommand, success: Boolean) extends ChowserExecuter.Result[TsvSliceCommand]

}
