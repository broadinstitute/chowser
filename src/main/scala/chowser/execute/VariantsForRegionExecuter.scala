package chowser.execute

import chowser.cmd.VariantsForRegionCommand
import chowser.execute.ChowserExecuter.Result
import chowser.filter.{Filter, RowFilters, StringFilters}
import chowser.genomics.Chromosome
import chowser.tsv.{BasicTsvReader, TsvUtils}
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsForRegionExecuter extends ChowserExecuter[VariantsForRegionCommand] {
  override def execute(command: VariantsForRegionCommand): Either[Snag, Result] = {
    import command.{inFile, outFile, chromColName, posColName, region}
    val rowFilter =
      RowFilters.ForCol(chromColName, ChromosomeFilter(region.chromosome)) &&
        RowFilters.ForCol(posColName, StringFilters.parseAsUnsignedIntegerAndFilter(_ >= region.start)) &&
        RowFilters.ForCol(posColName, StringFilters.parseAsUnsignedIntegerAndFilter(_ < region.end))
    TsvUtils.filterRows(inFile.file, outFile.file, BasicTsvReader.forSimpleHeaderLine(_), rowFilter)
    Right(Result.Done)
  }

  case class ChromosomeFilter(chromosome: Chromosome) extends Filter[String] {
    override def apply(chromosomeString: String): Boolean = {
      Chromosome.parse(chromosomeString) match {
        case Left(_) => false
        case Right(oChromosome) => oChromosome == chromosome
      }
    }
  }
}
