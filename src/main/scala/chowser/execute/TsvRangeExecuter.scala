package chowser.execute

import chowser.cmd.TsvRangeCommand
import chowser.execute.ChowserExecuter.Result
import chowser.filter.{RowFilters, StringFilters}
import chowser.tsv.{BasicTsvReader, TsvUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object TsvRangeExecuter extends ChowserExecuter[TsvRangeCommand] {

  override def execute(command: TsvRangeCommand): Either[Snag, Result] = {
    import command.{inFile, outFile, colName, filter}
    val rowFilter = RowFilters.ForCol(colName, StringFilters.parsesAsDoubleAndFilter(filter))
    TsvUtils.filterRows(inFile.file, outFile.file, BasicTsvReader.forSimpleHeaderLine(_), rowFilter)
    Right(Result.Done)
  }
}
