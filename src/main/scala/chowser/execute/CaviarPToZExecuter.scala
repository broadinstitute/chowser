package chowser.execute

import chowser.cmd.CaviarPToZCommand
import chowser.tsv.BasicTsvReader
import chowser.util.{MathUtils, NumberParser}

object CaviarPToZExecuter extends ChowserExecuter[CaviarPToZCommand] {

  def execute(command: CaviarPToZCommand): Result = {
    import command.{idCol, inFile, outFile, pCol}
    val reader = BasicTsvReader.forSimpleHeaderLine(inFile)
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    reader.foreach { row =>
      val id = row.valueMap(idCol)
      val pValueString = row.valueMap(pCol)
      val pValue = NumberParser.DoubleParser.parse(pValueString)
      val zScore = MathUtils.probit(pValue)
     outFile.appendLine(id + "\t" + zScore)
    }
    Result(command, success = true)
  }

  case class Result(command: CaviarPToZCommand, success: Boolean) extends ChowserExecuter.Result[CaviarPToZCommand]

}
