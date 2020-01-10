package chowser.execute

import chowser.cmd.VariantsMatchTsvTsvCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.genomics.VariantGroupId.VariantGroupIdTsvReader
import chowser.util.io.InputId
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsMatchTsvTsvExecuter extends ChowserExecuter[VariantsMatchTsvTsvCommand] {
  override def execute(command: VariantsMatchTsvTsvCommand): Either[Snag, Result] = {
    import command._
    val tsvToIter1: InputId => Iterator[VariantGroupId] = { file =>
      new VariantGroupIdTsvReader(idCol1)(file)
    }
    val tsvToIter2: InputId => Iterator[VariantGroupId] = { file =>
      new VariantGroupIdTsvReader(idCol2)(file)
    }
    val comparer = VariantMatcher(idCol1)
    comparer.compare(tsv1, tsv2, tsvToIter1, tsvToIter2, inBothOpt, onlyInOneOpt, onlyInTwoOpt)
    Right(Result.Done)
  }
}
