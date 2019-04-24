package chowser.execute

import better.files.File
import chowser.cmd.MatchVariantsCommand
import chowser.genomics
import chowser.genomics.VariantId
import htsjdk.variant.vcf.VCFFileReader

import scala.collection.JavaConverters._

object MatchVariantsExecuter extends ChowserExecuter[MatchVariantsCommand] {
  override def execute(command: MatchVariantsCommand): Result = {
    import command._
    val vcfToIter: File => Iterator[VariantId] = { file =>
      val reader = new VCFFileReader(file.path, false)
      reader.iterator().asScala.flatMap(VariantId.fromVariantContext).collect {
        case Right(variantIdLocation) => variantIdLocation
      }
    }
    val tsvToIter: File => Iterator[VariantId] = { file =>
      new genomics.VariantId.VariantIdTsvReader(idCol)(file)
    }
    val comparer = VariantMatcher(idCol)
    comparer.compare(vcf, tsv, vcfToIter, tsvToIter, inBothOpt, vcfOnlyOpt, tsvOnlyOpt)
    Result(command, success = true)
  }

  case class Result(command: MatchVariantsCommand, success: Boolean)
    extends ChowserExecuter.Result[MatchVariantsCommand]
}
