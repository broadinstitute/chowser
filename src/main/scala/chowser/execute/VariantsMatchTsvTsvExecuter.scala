package chowser.execute

import better.files.File
import chowser.cmd.VariantsMatchTsvTsvCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.genomics.VariantGroupId.VariantGroupIdTsvReader
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsMatchTsvTsvExecuter extends ChowserExecuter[VariantsMatchTsvTsvCommand] {
  override def execute(command: VariantsMatchTsvTsvCommand): Either[Snag, Result] = {
    import command._
    val tsvToIter1: File => Iterator[VariantGroupId] = { file =>
      new VariantGroupIdTsvReader(idCol1)(file)
    }
    val tsvToIter2: File => Iterator[VariantGroupId] = { file =>
      new VariantGroupIdTsvReader(idCol2)(file)
    }
    val comparer = VariantMatcher(idCol1)
    comparer.compare(tsv1.file, tsv2.file, tsvToIter1, tsvToIter2, inBothOpt.map(_.file), onlyInOneOpt.map(_.file),
      onlyInTwoOpt.map(_.file))
    Right(Result.Done)
  }
}
