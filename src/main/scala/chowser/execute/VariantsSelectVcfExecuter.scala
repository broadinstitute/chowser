package chowser.execute

import chowser.cmd.VariantsSelectVcfCommand
import chowser.genomics.VariantGroupId
import chowser.tsv.TsvUtils
import chowser.vcf.{VcfRecord, VcfUtils}

object VariantsSelectVcfExecuter extends ChowserExecuter[VariantsSelectVcfCommand] {

  def execute(command: VariantsSelectVcfCommand): Result = {
    import command._
    val selectedIds = TsvUtils.loadVariantGroupIds(selectionFile, idColSelection)
    val vcfRecordFilter: VcfRecord => Boolean = { record =>
      VariantGroupId.parse(record.id) match {
        case Left(message) =>
          println(message)
          false
        case Right(id) => selectedIds(id)
      }
    }
    VcfUtils.transformVcf(dataFile, outFile)(_.filter(vcfRecordFilter))
    Result(command, success = true)
  }

  case class Result(command: VariantsSelectVcfCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsSelectVcfCommand]

}
