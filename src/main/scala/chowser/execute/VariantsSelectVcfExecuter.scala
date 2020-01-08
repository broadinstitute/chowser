package chowser.execute

import chowser.cmd.VariantsSelectVcfCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.tsv.TsvUtils
import chowser.vcf.{VcfRecord, VcfUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsSelectVcfExecuter extends ChowserExecuter[VariantsSelectVcfCommand] {

  override def execute(command: VariantsSelectVcfCommand): Either[Snag, Result] = {
    import command._
    val selectedIds = TsvUtils.loadVariantGroupIds(selectionFile.file, idColSelection)
    val vcfRecordFilter: VcfRecord => Boolean = { record =>
      VariantGroupId.parse(record.id) match {
        case Left(message) =>
          println(message)
          false
        case Right(id) => selectedIds(id)
      }
    }
    VcfUtils.transformVcf(dataFile.file, outFile.file)(_.filter(vcfRecordFilter))
    Right(Result.Done)
  }
}
