package chowser.execute

import better.files.File
import chowser.cmd.CompareVariantsCommand
import chowser.tsv.TsvReader
import chowser.util.genomics.VariantIdLocation
import htsjdk.variant.vcf.VCFFileReader

import scala.collection.JavaConverters._

object CompareVariantsExecuter extends ChowserExecuter[CompareVariantsCommand] {
  override def execute(command: CompareVariantsCommand): Result = {
    import command._
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
    val comparer = new VariantComparerNew(idCol, chromCol, posCol)
    comparer.compare(vcf, tsv, vcfToIter, tsvToIter, inBothOpt, vcfOnlyOpt, tsvOnlyOpt)
    Result(command, success = true)
  }

  case class Result(command: CompareVariantsCommand, success: Boolean)
    extends ChowserExecuter.Result[CompareVariantsCommand]
}
