package chowser.execute

import chowser.cmd.VariantsSelectTsvCommand
import chowser.genomics.VariantGroupId
import chowser.tsv.{BasicTsvReader, TsvUtils}
import chowser.tsv.BasicTsvReader.Row

object VariantsSelectTsvExecuter extends ChowserExecuter[VariantsSelectTsvCommand] {

  def execute(command: VariantsSelectTsvCommand): Result = {
    import command._
    val selectedIds = TsvUtils.loadVariantGroupIds(selectionFile, idColSelection)
    val rowFilter: Row => Boolean = { row =>
      row.valueMap.get(idColData).map(VariantGroupId.parse) match {
        case Some(Right(id)) => selectedIds(id)
        case _ => false
      }
    }
    val readerData = BasicTsvReader.forSimpleHeaderLine(dataFile)
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    readerData.headerLines.foreach(outFile.appendLine(_))
    readerData.filter(rowFilter).map(_.line).foreach(outFile.appendLine(_))
    Result(command, success = true)
  }

  case class Result(command: VariantsSelectTsvCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsSelectTsvCommand]

}
