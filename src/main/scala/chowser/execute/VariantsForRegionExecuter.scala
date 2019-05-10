package chowser.execute

import chowser.cmd.VariantsForRegionCommand
import chowser.filter.{Filter, RowFilters, StringFilters}
import chowser.genomics.Chromosome
import chowser.tsv.{TsvReader, TsvUtils}

object VariantsForRegionExecuter extends ChowserExecuter[VariantsForRegionCommand] {
  override def execute(command: VariantsForRegionCommand): ChowserExecuter.Result[VariantsForRegionCommand] = {
    import command.{inFile, outFile, chromColName, posColName, region}
    val rowFilter =
      RowFilters.ForCol(chromColName, ChromosomeFilter(region.chromosome)) &&
        RowFilters.ForCol(posColName, StringFilters.parseAsUnsignedIntegerAndFilter(_ >= region.start)) &&
        RowFilters.ForCol(posColName, StringFilters.parseAsUnsignedIntegerAndFilter(_ < region.end))
    TsvUtils.filterRows(inFile, outFile, TsvReader.forSimpleHeaderLine(_), rowFilter)
    Result(command, success = true)
  }

  case class Result(command: VariantsForRegionCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsForRegionCommand]

  case class ChromosomeFilter(chromosome: Chromosome) extends Filter[String] {
    override def apply(chromosomeString: String): Boolean = {
      Chromosome.parse(chromosomeString) match {
        case Left(_) => false
        case Right(oChromosome) => oChromosome == chromosome
      }
    }
  }
}
