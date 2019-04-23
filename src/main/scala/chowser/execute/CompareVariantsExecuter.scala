package chowser.execute

import better.files.File
import chowser.cmd.CompareVariantsCommand
import chowser.tsv.TsvReader
import chowser.genomics.VariantIdOld
import htsjdk.variant.vcf.VCFFileReader

import scala.collection.JavaConverters._

object CompareVariantsExecuter extends ChowserExecuter[CompareVariantsCommand] {
  override def execute(command: CompareVariantsCommand): Result = {
    import command._
    val vcfToIter: File => Iterator[VariantIdOld] = { file =>
      val reader = new VCFFileReader(file.path, false)
      reader.iterator().asScala.map(VariantIdOld.fromVariantContext).collect {
        case Right(variantIdLocation) => variantIdLocation
      }
    }
    val tsvToIter: File => Iterator[VariantIdOld] = { file =>
      TsvReader.forSimpleHeaderLine(file).map { row =>
        VariantIdOld.fromMap(idCol, chromCol, posCol)(row.valueMap)
      }.collect {
        case Right(variantIdLocation) => variantIdLocation
      }
    }
    val comparer = VariantComparer(idCol, chromCol, posCol)
    comparer.compare(vcf, tsv, vcfToIter, tsvToIter, inBothOpt, vcfOnlyOpt, tsvOnlyOpt)
    Result(command, success = true)
  }

  case class Result(command: CompareVariantsCommand, success: Boolean)
    extends ChowserExecuter.Result[CompareVariantsCommand]
}
