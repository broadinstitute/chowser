package chowser.execute

import better.files.File
import chowser.cmd.VariantsRegionsCommand
import chowser.tsv.{TsvReader, TsvWriter}
import org.scalatest.FunSuite

class VariantsRegionsExecuterTest extends FunSuite {

  private val chromColName = "chrom"
  private val posColName = "pos"
  private val radius = 1000


  private def prepareInFile(file: File): Unit = {
    val writer = TsvWriter(file, Seq(chromColName, posColName))
    writer.addRow("1", "104200")
    writer.addRow("1", "103500")
    writer.addRow("1", "101400")
    writer.addRow("1", "100700")
    writer.addRow("1", "102800")
    writer.addRow("1", "102100")
    writer.addRow("3", "104200")
    writer.addRow("3", "103500")
    writer.addRow("3", "101400")
    writer.addRow("3", "100700")
    writer.addRow("3", "102800")
    writer.addRow("3", "102100")
    writer.addRow("X", "104200")
    writer.addRow("X", "103500")
    writer.addRow("X", "101400")
    writer.addRow("X", "100700")
    writer.addRow("X", "102800")
    writer.addRow("X", "102100")
    writer.addRow("1", "204200")
    writer.addRow("1", "203500")
    writer.addRow("1", "201400")
    writer.addRow("1", "200700")
    writer.addRow("1", "202800")
    writer.addRow("1", "202100")
  }

  test("execute") {
    val rootDir = File.newTemporaryDirectory()
    val inFile = rootDir / "inFile"
    val outFile = rootDir / "outFile"
    prepareInFile(inFile)
    val command = VariantsRegionsCommand(inFile, outFile, chromColName, posColName, radius)
    val result = VariantsRegionsExecuter.execute(command)
    assert(result.success)
    val reReader = TsvReader.forSimpleHeaderLine(outFile)
    val rows = reReader.toSeq
    val chromosomes = rows.map(row => row.valueMap(chromColName))
    assert(chromosomes == Seq("1", "1", "3", "X"))
    val starts = rows.map(row => row.valueMap(VariantsRegionsExecuter.startColName))
    assert(starts == Seq("99700", "199700", "99700", "99700"))
    val ends = rows.map(row => row.valueMap(VariantsRegionsExecuter.endColName))
    assert(ends == Seq("105200", "205200", "105200", "105200"))
  }

}
