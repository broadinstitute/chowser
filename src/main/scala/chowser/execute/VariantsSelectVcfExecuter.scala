package chowser.execute

import chowser.cmd.VariantsSelectVcfCommand
import chowser.genomics.VariantGroupId
import chowser.tsv.TsvUtils
import chowser.vcf.HtsjdkUtils
import htsjdk.variant.variantcontext.VariantContext

object VariantsSelectVcfExecuter extends ChowserExecuter[VariantsSelectVcfCommand] {

  def execute(command: VariantsSelectVcfCommand): Result = {
    import command._
    val selectedIds = TsvUtils.loadVariantGroupIds(selectionFile, idColSelection)
    val variantContextFilter: VariantContext => Boolean = { context =>
      VariantGroupId.parse(context.getContig) match {
        case Right(id) => selectedIds(id)
        case Left(message) =>
          println(message)
          false
      }
    }
    HtsjdkUtils.transformVcf(dataFile, outFile)(_.filter(variantContextFilter))
    Result(command, success = true)
  }

  case class Result(command: VariantsSelectVcfCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsSelectVcfCommand]

}
