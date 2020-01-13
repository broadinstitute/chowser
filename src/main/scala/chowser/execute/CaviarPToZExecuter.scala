package chowser.execute

import chowser.cmd.CaviarPToZCommand
import chowser.execute.ChowserExecuter.Result
import chowser.tsv.BasicTsvReader
import chowser.util.{MathUtils, NumberParser}
import org.broadinstitute.yootilz.core.snag.Snag

object CaviarPToZExecuter extends ChowserExecuter[CaviarPToZCommand] {

  override def execute(command: CaviarPToZCommand): Either[Snag, Result] = {
    import command._
    val reader = BasicTsvReader.forSimpleHeaderLine(inFile, resourceConfig)
    val writer = outFile.newPrintWriter(resourceConfig)
    reader.foreach { row =>
      val id = row.valueMap(idCol)
      val pValueString = row.valueMap(pCol)
      val pValue = NumberParser.DoubleParser.parse(pValueString)
      val zScore = MathUtils.probit(pValue)
      writer.println(id + "\t" + zScore)
    }
    Right(Result.Done)
  }
}
