package chowser.execute

import chowser.cmd.VariantsForRegionByIdCommand
import chowser.filter.{Filter, RowFilters}
import chowser.genomics.{Region, VariantId}
import chowser.tsv.{TsvReader, TsvUtils}

object VariantsForRegionByIdExecuter extends ChowserExecuter[VariantsForRegionByIdCommand] {
  override def execute(command: VariantsForRegionByIdCommand): ChowserExecuter.Result[VariantsForRegionByIdCommand] = {
    import command.{idColName, inFile, outFile, region}
    val rowFilter = RowFilters.ForCol(idColName, VariantInRegionFilter(region))
    TsvUtils.filterRows(inFile, outFile, TsvReader.forSimpleHeaderLine(_), rowFilter)
    Result(command, success = true)
  }

  case class Result(command: VariantsForRegionByIdCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsForRegionByIdCommand]

  case class VariantInRegionFilter(region: Region) extends Filter[String] {
    override def apply(idString: String): Boolean = {
      VariantId.parse(idString) match {
        case Left(_) => false
        case Right(id) => region.includes(id.location)
      }
    }
  }

}
