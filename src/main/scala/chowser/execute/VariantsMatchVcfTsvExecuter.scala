package chowser.execute

import chowser.cmd.VariantsMatchVcfTsvCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.genomics.VariantGroupId.VariantGroupIdTsvReader
import chowser.util.io.{FileInputId, InputId}
import htsjdk.variant.vcf.VCFFileReader
import org.broadinstitute.yootilz.core.snag.Snag

import scala.jdk.CollectionConverters._

object VariantsMatchVcfTsvExecuter extends ChowserExecuter[VariantsMatchVcfTsvCommand] {
  override def execute(command: VariantsMatchVcfTsvCommand): Either[Snag, Result] = {
    import command._
    if (vcf.isInstanceOf[FileInputId]) {
      val vcfToIter: InputId => Iterator[VariantGroupId] = { file =>
        val reader = new VCFFileReader(file.asInstanceOf[FileInputId].file.path, false)
        reader.iterator().asScala.map(VariantGroupId.fromVariantContext).collect {
          case Right(variantGroupId) => variantGroupId
        }
      }
      val tsvToIter: InputId => Iterator[VariantGroupId] = { file =>
        new VariantGroupIdTsvReader(idCol)(file, resourceConfig)
      }
      val comparer = VariantMatcher(idCol)
      comparer.compare(vcf, tsv, vcfToIter, tsvToIter, inBothOpt, vcfOnlyOpt, tsvOnlyOpt, resourceConfig)
      Right(Result.Done)
    } else {
      Left(Snag(s"For this feature, VCF file needs to be local file, but $vcf is not."))
    }
  }
}
