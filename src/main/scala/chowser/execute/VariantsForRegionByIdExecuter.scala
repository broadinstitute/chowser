package chowser.execute

import chowser.cmd.VariantsForRegionByIdCommand
import chowser.execute.ChowserExecuter.Result
import chowser.filter.{Filter, RowFilters}
import chowser.genomics.{Region, VariantId}
import chowser.tsv.{BasicTsvReader, TsvUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsForRegionByIdExecuter extends ChowserExecuter[VariantsForRegionByIdCommand] {
  override def execute(command: VariantsForRegionByIdCommand): Either[Snag, Result] = {
    import command.{idColName, inFile, outFile, region}
    val rowFilter = RowFilters.ForCol(idColName, VariantInRegionFilter(region))
    TsvUtils.filterRows(inFile, outFile, BasicTsvReader.forSimpleHeaderLine(_), rowFilter)
    Right(Result.Done)
  }

  case class VariantInRegionFilter(region: Region) extends Filter[String] {
    override def apply(idString: String): Boolean = {
      VariantId.parse(idString) match {
        case Left(_) => false
        case Right(id) => region.includes(id.location)
      }
    }
  }

}
