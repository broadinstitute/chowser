package chowser.execute

import chowser.cmd.LiftoverTsvCommand
import chowser.tsv.{BasicTsvReader, TsvRow, TsvWriter}
import htsjdk.samtools.liftover.LiftOver

object LiftoverTsvExecuter extends ChowserExecuter[LiftoverTsvCommand] {

  def execute(command: LiftoverTsvCommand): Result = {
    import command.{chainFile, inFile, outFile}
    val liftOver = new LiftOver(chainFile.toJava)
    val reader = BasicTsvReader.forSimpleHeaderLine(inFile)
    val writer = TsvWriter(outFile, reader.headerLines)
    val rowMapper: TsvRow => Iterator[TsvRow] = { row =>
      ???
    }
    reader.flatMap(rowMapper).foreach(row => writer.addRow(row.valueMap))
    Result(command, success = true)
  }

  case class Result(command: LiftoverTsvCommand, success: Boolean) extends ChowserExecuter.Result[LiftoverTsvCommand]

}
