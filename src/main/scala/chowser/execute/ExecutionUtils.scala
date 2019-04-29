package chowser.execute

import better.files.File
import chowser.filter.Filter
import chowser.tsv.TsvReader
import chowser.tsv.TsvReader.Row

import scala.sys.process.Process

object ExecutionUtils {

  def filterRows(inFile: File, outFile: File, readerGenerator: File => TsvReader, filter: Filter[Row]): Unit = {
    val reader = readerGenerator(inFile)
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    reader.headerLines.foreach(outFile.appendLine(_))
    reader.filter(filter).map(_.line).foreach(outFile.appendLine(_))
  }

  def sortRowsByCol(inFile: File, outFile: File, readerGenerator: File => TsvReader, colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    val nHeaderLines = reader.headerLines.size
    val commandString =
      s"{ head -n $nHeaderLines $inFile; tail -n +${nHeaderLines + 1} $inFile | " +
        s"sort -t$$'\\t' -k${colIndex + 1} -n ; } > $outFile"
    println(commandString)
    runBashScript(commandString)
  }

  def extractUniqueValues(inFile: File, outFile: File, readerGenerator: File => TsvReader, colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    val nHeaderLines = reader.headerLines.size
    val commandString =
      s"{ echo $colName; tail -n +${nHeaderLines + 1} $inFile | cut -f ${colIndex + 1} | sort -u ; } > $outFile"
    println(commandString)
    runBashScript(commandString)

  }

  def runBashScript(script: String): Unit = {
    File.usingTemporaryFile() { scriptFile =>
      scriptFile.overwrite(script)
      Process(Seq("bash", scriptFile.toString())).!
    }
  }

}
