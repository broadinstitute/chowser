package chowser.execute

import chowser.cmd.VariantsSelectTsvCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.tsv.{BasicTsvReader, TsvRow, TsvUtils, TsvWriter}
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsSelectTsvExecuter extends ChowserExecuter[VariantsSelectTsvCommand] {

  override def execute(command: VariantsSelectTsvCommand): Either[Snag, Result] = {
    import command._
    val selectedIds = TsvUtils.loadVariantGroupIds(selectionFile, idColSelection)
    val rowFilter: TsvRow => Boolean = { row =>
      row.valueMap.get(idColData).map(VariantGroupId.parse) match {
        case Some(Right(id)) => selectedIds(id)
        case _ => false
      }
    }
    val readerData = BasicTsvReader.forSimpleHeaderLine(dataFile)
    val writer = TsvWriter(outFile, readerData.header)
    readerData.filter(rowFilter).foreach(writer.addRow)
    Right(Result.Done)
  }
}
