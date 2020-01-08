package chowser.execute

import chowser.cmd.VariantsSelectTsvCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.tsv.{BasicTsvReader, TsvUtils}
import chowser.tsv.TsvRow
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
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    readerData.header.lines.foreach(outFile.appendLine(_))
    readerData.filter(rowFilter).map(_.line).foreach(outFile.appendLine(_))
    Right(Result.Done)
  }
}
