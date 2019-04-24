package chowser.execute

import better.files.File
import chowser.cmd.CompareVariantsCommand
import chowser.genomics
import chowser.genomics.VariantId
import htsjdk.variant.vcf.VCFFileReader

import scala.collection.JavaConverters._

object CompareVariantsExecuter extends ChowserExecuter[CompareVariantsCommand] {
  override def execute(command: CompareVariantsCommand): Result = {
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
    val comparer = VariantComparer(idCol, chromCol, posCol)
    comparer.compare(vcf, tsv, vcfToIter, tsvToIter, inBothOpt, vcfOnlyOpt, tsvOnlyOpt)
    Result(command, success = true)
  }

  case class Result(command: CompareVariantsCommand, success: Boolean)
    extends ChowserExecuter.Result[CompareVariantsCommand]
}
