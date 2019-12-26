package chowser.tsv

import better.files.File
import chowser.execute.ExecutionUtils
import chowser.filter.Filter
import chowser.genomics.VariantGroupId

object TsvUtils {

  def filterRows(inFile: File, outFile: File, readerGenerator: File => BasicTsvReader, filter: Filter[TsvRow]): Unit = {
    val reader = readerGenerator(inFile)
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    reader.header.lines.foreach(outFile.appendLine(_))
    reader.filter(filter).map(_.line).foreach(outFile.appendLine(_))
  }

  def sortRowsByCol(inFile: File, outFile: File, readerGenerator: File => BasicTsvReader, colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    val nHeaderLines = reader.header.lines.size
    val commandString =
      s"{ head -n $nHeaderLines $inFile; tail -n +${nHeaderLines + 1} $inFile | " +
        s"sort -t$$'\\t' -k${colIndex + 1} -n ; } > $outFile"
    println(commandString)
    ExecutionUtils.runBashScript(commandString)
  }

  def sortRowsByIds(inFile: File, outFile: File, readerGenerator: File => BasicTsvReader, colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    if (outFile.nonEmpty) {
      outFile.clear()
    }
//    val nHeaderLines = reader.header.lines.size
//    val commandString =
//      s"{ head -n $nHeaderLines $inFile; tail -n +${nHeaderLines + 1} $inFile | " +
//        s"sort -t$$'\\t' -k${colIndex + 1} -n ; } > $outFile"
//    println(commandString)
//    ExecutionUtils.runBashScript(commandString)
    ???
  }

  def extractUniqueValues(inFile: File, outFile: File, readerGenerator: File => BasicTsvReader, colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    val nHeaderLines = reader.header.lines.size
    val commandString =
      s"{ echo $colName; tail -n +${nHeaderLines + 1} $inFile | cut -f ${colIndex + 1} | sort -u ; } > $outFile"
    println(commandString)
    ExecutionUtils.runBashScript(commandString)

  }

  def loadVariantGroupIds(file: File, idCol: String): Set[VariantGroupId] = {
    val readerSelection = BasicTsvReader.forSimpleHeaderLine(file)
    val selectedIdStrings = readerSelection.flatMap(_.valueMap.get(idCol))
    selectedIdStrings.map(VariantGroupId.parse).collect {
      case Right(id) => id
    }.toSet
  }
}
