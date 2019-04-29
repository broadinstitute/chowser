package chowser.execute

import chowser.cmd.VariantsRegionsCommand
import chowser.tsv.{TsvReader, TsvWriter}
import chowser.util.intervals.{CanonicalIntervals, Interval}

object VariantsRegionsExecuter extends ChowserExecuter[VariantsRegionsCommand] {

  val startColName = "start"
  val endColName = "end"

  def execute(command: VariantsRegionsCommand): Result = {
    import command.{inFile, outFile, chromColName, posColName, radius }
    val rowIterator = TsvReader.forSimpleHeaderLine(inFile)
    var regionsByChromosome: Map[String, CanonicalIntervals] = Map.empty
    for(row <- rowIterator) {
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
    if(outFile.nonEmpty) {
      outFile.clear()
    }
    val writer = TsvWriter(outFile, Seq(chromColName, startColName, endColName))
    for(chromosome <- chromosomes) {
      val regions = regionsByChromosome(chromosome)
      for(region <- regions.intervals) {
        writer.addRow(chromosome, region.start.toString, region.end.toString)
      }
    }
    Result(command, success = true)
  }

  case class Result(command: VariantsRegionsCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsRegionsCommand]

}
