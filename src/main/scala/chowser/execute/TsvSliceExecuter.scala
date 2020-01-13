package chowser.execute

import chowser.cmd.TsvSliceCommand
import chowser.execute.ChowserExecuter.Result
import chowser.filter.RowFilters
import chowser.tsv.{BasicTsvReader, TsvUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object TsvSliceExecuter extends ChowserExecuter[TsvSliceCommand] {

  override def execute(command: TsvSliceCommand): Either[Snag, Result] = {
    import command.{colName, filter, inFile, outFile, resourceConfig}
    val rowFilter = RowFilters.ForCol(colName, filter)
    TsvUtils.filterRows(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_, resourceConfig), resourceConfig,
      rowFilter)
    Right(Result.Done)
  }
}
