package chowser.execute

import chowser.cmd.VariantsRegionsCommand
import chowser.execute.ChowserExecuter.Result
import chowser.tsv.{BasicTsvReader, TsvHeader, TsvRow, TsvWriter}
import chowser.util.intervals.{CanonicalIntervals, Interval}
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsRegionsExecuter extends ChowserExecuter[VariantsRegionsCommand] {

  val startColName = "start"
  val endColName = "end"

  override def execute(command: VariantsRegionsCommand): Either[Snag, Result] = {
    import command.{inFile, outFile, chromColName, posColName, radius, resourceConfig}
    val rowIterator = BasicTsvReader.forSimpleHeaderLine(inFile, resourceConfig)
    var regionsByChromosome: Map[String, CanonicalIntervals] = Map.empty
    for (row <- rowIterator) {
      val chromosome = row.string(chromColName)
      val regionsOld = regionsByChromosome.getOrElse(chromosome, CanonicalIntervals.empty)
      val position = row.unsignedInt(posColName)
      val regionStart = Math.max(0, position - radius)
      val regionEnd = position + radius
      val region = Interval(regionStart, regionEnd)
      val regionsNew = regionsOld :+ region
      regionsByChromosome += (chromosome -> regionsNew)
    }
    val chromosomes = regionsByChromosome.keys.toSeq.sorted
    val writer = TsvWriter(outFile, TsvHeader.ofColNames(Seq(chromColName, startColName, endColName)), resourceConfig)
    for (chromosome <- chromosomes) {
      val regions = regionsByChromosome(chromosome)
      for (region <- regions.intervals) {
        writer.addRow(
          TsvRow(chromColName -> chromosome, startColName -> region.start.toString, endColName -> region.end.toString)
        )
      }
    }
    Right(Result.Done)
  }
}
