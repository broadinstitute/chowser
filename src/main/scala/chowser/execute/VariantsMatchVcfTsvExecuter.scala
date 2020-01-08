package chowser.execute

import better.files.File
import chowser.cmd.VariantsMatchVcfTsvCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.genomics.VariantGroupId.VariantGroupIdTsvReader
import htsjdk.variant.vcf.VCFFileReader
import org.broadinstitute.yootilz.core.snag.Snag

import scala.jdk.CollectionConverters._

object VariantsMatchVcfTsvExecuter extends ChowserExecuter[VariantsMatchVcfTsvCommand] {
  override def execute(command: VariantsMatchVcfTsvCommand): Either[Snag, Result] = {
    import command._
    val vcfToIter: File => Iterator[VariantGroupId] = { file =>
      val reader = new VCFFileReader(file.path, false)
      reader.iterator().asScala.map(VariantGroupId.fromVariantContext).collect {
        case Right(variantGroupId) => variantGroupId
      }
    }
    val tsvToIter: File => Iterator[VariantGroupId] = { file =>
      new VariantGroupIdTsvReader(idCol)(file)
    }
    val comparer = VariantMatcher(idCol)
    comparer.compare(vcf, tsv, vcfToIter, tsvToIter, inBothOpt, vcfOnlyOpt, tsvOnlyOpt)
    Right(Result.Done)
  }
}
