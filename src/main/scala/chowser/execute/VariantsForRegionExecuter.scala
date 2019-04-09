package chowser.execute

import chowser.cmd.VariantsForRegionCommand
import chowser.filter.{RowFilters, StringFilters}
import chowser.tsv.TsvReader

object VariantsForRegionExecuter extends ChowserExecuter[VariantsForRegionCommand] {
  override def execute(command: VariantsForRegionCommand): ChowserExecuter.Result[VariantsForRegionCommand] = {
    import command.{inFile, outFile, chromColName, posColName, chromosome, start, end}
    val rowFilter =
      RowFilters.ForCol(chromColName, _ == chromosome) &&
        RowFilters.ForCol(posColName, StringFilters.parseAsUnsignedIntegerAndFilter(_ >= start)) &&
        RowFilters.ForCol(posColName, StringFilters.parseAsUnsignedIntegerAndFilter(_ < end))
    RowFilters.ForCol(posColName, StringFilters.parsesAsUnsignedIntegerFilter)
    ExecutionUtils.filterRows(inFile, outFile, TsvReader.forSimpleHeaderLine(_), rowFilter)
    Result(command, success = true)
  }

  case class Result(command: VariantsForRegionCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsForRegionCommand]
}
