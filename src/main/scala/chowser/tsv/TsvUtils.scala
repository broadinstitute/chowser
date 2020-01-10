package chowser.tsv

import better.files.File
import chowser.execute.ExecutionUtils
import chowser.filter.Filter
import chowser.genomics.VariantGroupId
import chowser.util.io.{InputId, OutputId}

import scala.collection.immutable.TreeSet

object TsvUtils {

  def filterRows(inFile: InputId, outFile: OutputId, readerGenerator: InputId => BasicTsvReader,
                 filter: Filter[TsvRow]): Unit = {
    val reader = readerGenerator(inFile)
    if (outFile.file.nonEmpty) {
      outFile.file.clear()
    }
    reader.header.lines.foreach(outFile.file.appendLine(_))
    reader.filter(filter).map(_.line).foreach(outFile.file.appendLine(_))
  }

  def sortRowsByCol(inFile: InputId, outFile: OutputId, readerGenerator: InputId => BasicTsvReader,
                    colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    if (outFile.file.nonEmpty) {
      outFile.file.clear()
    }
    val nHeaderLines = reader.header.lines.size
    val commandString =
      s"{ head -n $nHeaderLines $inFile; tail -n +${nHeaderLines + 1} $inFile | " +
        s"sort -t$$'\\t' -k${colIndex + 1} -n ; } > $outFile"
    println(commandString)
    ExecutionUtils.runBashScript(commandString)
  }

  def sortRowsByIds(inFile: InputId, outFile: OutputId, readerGenerator: InputId => BasicTsvReader,
                    colName: String): Unit = {
    val reader = readerGenerator(inFile)
    if (!reader.cols.contains(colName)) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    if (outFile.file.nonEmpty) {
      outFile.file.clear()
    }
    val orderingByIds = new Ordering[TsvRow] {
      override def compare(row1: TsvRow, row2: TsvRow): Int = {
        val location1 = VariantGroupId.parse(row1.valueMap(colName)).toOption.get.location
        val location2 = VariantGroupId.parse(row2.valueMap(colName)).toOption.get.location
        location1.compare(location2)
      }
    }
    val rowsSorted = TreeSet[TsvRow]()(orderingByIds) ++ reader
    val writer = TsvWriter(outFile, reader.header)
    rowsSorted.foreach(writer.addRow)
  }

  def extractUniqueValues(inFile: InputId, outFile: OutputId, readerGenerator: InputId => BasicTsvReader,
                          colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    if (outFile.file.nonEmpty) {
      outFile.file.clear()
    }
    val nHeaderLines = reader.header.lines.size
    val commandString =
      s"{ echo $colName; tail -n +${nHeaderLines + 1} $inFile | cut -f ${colIndex + 1} | sort -u ; } > $outFile"
    println(commandString)
    ExecutionUtils.runBashScript(commandString)

  }

  def loadVariantGroupIds(file: InputId, idCol: String): Set[VariantGroupId] = {
    val readerSelection = BasicTsvReader.forSimpleHeaderLine(file)
    val selectedIdStrings = readerSelection.flatMap(_.valueMap.get(idCol))
    selectedIdStrings.map(VariantGroupId.parse).collect {
      case Right(id) => id
    }.toSet
  }
}
