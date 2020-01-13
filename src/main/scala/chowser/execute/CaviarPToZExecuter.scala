package chowser.execute

import chowser.cmd.CaviarPToZCommand
import chowser.execute.ChowserExecuter.Result
import chowser.tsv.BasicTsvReader
import chowser.util.{MathUtils, NumberParser}
import org.broadinstitute.yootilz.core.snag.Snag

object CaviarPToZExecuter extends ChowserExecuter[CaviarPToZCommand] {

  override def execute(command: CaviarPToZCommand): Either[Snag, Result] = {
    import command.{idCol, inFile, outFile, pCol}
    val reader = BasicTsvReader.forSimpleHeaderLine(inFile)
    if (outFile.fileDeprecated.nonEmpty) {
      outFile.fileDeprecated.clear()
    }
    reader.foreach { row =>
      val id = row.valueMap(idCol)
      val pValueString = row.valueMap(pCol)
      val pValue = NumberParser.DoubleParser.parse(pValueString)
      val zScore = MathUtils.probit(pValue)
     outFile.appendLine(id + "\t" + zScore)
    }
    Right(Result.Done)
  }
}
