package chowser.execute

import chowser.cmd.TsvSortIdsCommand
import chowser.execute.ChowserExecuter.Result
import chowser.tsv.{BasicTsvReader, TsvUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object TsvSortIdsExecuter extends ChowserExecuter[TsvSortIdsCommand] {

  override def execute(command: TsvSortIdsCommand): Either[Snag, Result] = {
    import command.{colName, inFile, outFile}
    TsvUtils.sortRowsByIds(inFile.file, outFile.file, BasicTsvReader.forSimpleHeaderLine(_), colName)
    Right(Result.Done)
  }
}
