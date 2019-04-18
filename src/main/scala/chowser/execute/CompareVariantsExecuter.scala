package chowser.execute

import chowser.cmd.{CompareVariantsCommand, VariantsForRegionCommand}
import chowser.filter.{RowFilters, StringFilters}
import chowser.tsv.TsvReader
import chowser.util.genomics.VariantIdLocation
import better.files.File
import htsjdk.variant.vcf.VCFFileReader
import scala.collection.JavaConverters._

object CompareVariantsExecuter extends ChowserExecuter[CompareVariantsCommand] {
  override def execute(command: CompareVariantsCommand): Result = {
    import command.{vcf, tsv, idCol, chromCol, posCol, inBothOpt, vcfOnlyOpt, tsvOnlyOpt}
    val vcfToIter: File => Iterator[VariantIdLocation] = { file =>
      val reader = new VCFFileReader(file.path, false)
      reader.iterator().asScala.map(VariantIdLocation.fromVariantContext).collect {
        case Right(variantIdLocation) => variantIdLocation
      }
    }
    val tsvToIter: File => Iterator[VariantIdLocation] = { file =>
      TsvReader.forSimpleHeaderLine(file).map { row =>
        VariantIdLocation.fromMap(idCol, chromCol, posCol)(row.valueMap)
      }.collect {
        case Right(variantIdLocation) => variantIdLocation
      }
    }
    VariantComparer.compare(vcf, tsv, vcfToIter, tsvToIter, inBothOpt, vcfOnlyOpt, tsvOnlyOpt)
    Result(command, success = true)
  }

  case class Result(command: CompareVariantsCommand, success: Boolean)
    extends ChowserExecuter.Result[CompareVariantsCommand]
}
