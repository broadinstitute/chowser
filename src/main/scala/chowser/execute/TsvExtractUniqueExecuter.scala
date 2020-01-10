package chowser.execute

import chowser.cmd.TsvExtractUniqueCommand
import chowser.execute.ChowserExecuter.Result
import chowser.tsv.{BasicTsvReader, TsvUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object TsvExtractUniqueExecuter extends ChowserExecuter[TsvExtractUniqueCommand] {

  override def execute(command: TsvExtractUniqueCommand): Either[Snag, Result] = {
    import command.{colName, inFile, outFile}
    TsvUtils.extractUniqueValues(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_), colName)
    Right(Result.Done)
  }

}
