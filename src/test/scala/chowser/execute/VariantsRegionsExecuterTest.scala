package chowser.execute

import better.files.File
import chowser.cmd.VariantsRegionsCommand
import chowser.tsv.{BasicTsvReader, TsvHeader, TsvRow, TsvWriter}
import org.scalatest.FunSuite

class VariantsRegionsExecuterTest extends FunSuite {

  private val chromColName = "chrom"
  private val posColName = "pos"
  private val radius = 1000


  private def prepareInFile(file: File): Unit = {
    val writer = TsvWriter(file, TsvHeader.ofColNames(Seq(chromColName, posColName)))
    def addRow(chrom: String, pos: String): Unit = writer.addRow(TsvRow(chromColName -> chrom, posColName -> pos))
    addRow("1", "104200")
    addRow("1", "103500")
    addRow("1", "101400")
    addRow("1", "100700")
    addRow("1", "102800")
    addRow("1", "102100")
    addRow("3", "104200")
    addRow("3", "103500")
    addRow("3", "101400")
    addRow("3", "100700")
    addRow("3", "102800")
    addRow("3", "102100")
    addRow("X", "104200")
    addRow("X", "103500")
    addRow("X", "101400")
    addRow("X", "100700")
    addRow("X", "102800")
    addRow("X", "102100")
    addRow("1", "204200")
    addRow("1", "203500")
    addRow("1", "201400")
    addRow("1", "200700")
    addRow("1", "202800")
    addRow("1", "202100")
  }

  test("execute") {
    val rootDir = File.newTemporaryDirectory()
    val inFile = rootDir / "inFile"
    val outFile = rootDir / "outFile"
    prepareInFile(inFile)
    val command = VariantsRegionsCommand(inFile, outFile, chromColName, posColName, radius)
    val result = VariantsRegionsExecuter.execute(command)
    assert(result.success)
    val reReader = BasicTsvReader.forSimpleHeaderLine(outFile)
    val rows = reReader.toSeq
    val chromosomes = rows.map(row => row.valueMap(chromColName))
    assert(chromosomes == Seq("1", "1", "3", "X"))
    val starts = rows.map(row => row.valueMap(VariantsRegionsExecuter.startColName))
    assert(starts == Seq("99700", "199700", "99700", "99700"))
    val ends = rows.map(row => row.valueMap(VariantsRegionsExecuter.endColName))
    assert(ends == Seq("105200", "205200", "105200", "105200"))
  }

}
