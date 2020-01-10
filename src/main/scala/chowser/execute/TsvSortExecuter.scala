package chowser.execute

import chowser.cmd.TsvSortCommand
import chowser.execute.ChowserExecuter.Result
import chowser.tsv.{BasicTsvReader, TsvUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object TsvSortExecuter extends ChowserExecuter[TsvSortCommand] {

  override def execute(command: TsvSortCommand): Either[Snag, Result] = {
    import command.{colName, inFile, outFile}
    TsvUtils.sortRowsByCol(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_), colName)
    Right(Result.Done)
  }
}
