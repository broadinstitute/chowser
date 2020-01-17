package chowser.tsv

import chowser.execute.ExecutionUtils
import chowser.filter.Filter
import chowser.genomics.VariantGroupId
import chowser.util.io._

import scala.collection.immutable.TreeSet

object TsvUtils {

  def filterRows(inFile: InputId, outFile: OutputId, readerGenerator: InputId => BasicTsvReader,
                 resourceConfig: ResourceConfig, filter: Filter[TsvRow]): Unit = {
    val reader = readerGenerator(inFile)
    val writer = TsvWriter(outFile, reader.header, resourceConfig)
    reader.filter(filter).foreach(writer.addRow)
    writer.writer.close()
  }

  def sortRowsByCol(inFile: InputId, outFile: OutputId, readerGenerator: InputId => BasicTsvReader,
                    colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    inFile match {
      case FileInputId(inFileFile) =>
        outFile match {
          case FileOutputId(outFileFile) =>
            val nHeaderLines = reader.header.lines.size
            val commandString =
              s"{ head -n $nHeaderLines $inFileFile; tail -n +${nHeaderLines + 1} $inFileFile | " +
                s"sort -t$$'\\t' -k${colIndex + 1} -n ; } > $outFileFile"
            println(commandString)
            ExecutionUtils.runBashScript(commandString)
          case _ => throw new Exception(s"Output file for this feature needs to be local file, but $outFile is not.")
        }
      case _ => throw new Exception(s"Input file for this feature needs to be local file, but $inFile is not.")
    }
  }

  def sortRowsByIds(inFile: InputId, outFile: OutputId, readerGenerator: InputId => BasicTsvReader,
                    colName: String, resourceConfig: ResourceConfig): Unit = {
    val reader = readerGenerator(inFile)
    if (!reader.cols.contains(colName)) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    val orderingByIds = new Ordering[TsvRow] {
      override def compare(row1: TsvRow, row2: TsvRow): Int = {
        val location1 = VariantGroupId.parse(row1.valueMap(colName)).toOption.get.location
        val location2 = VariantGroupId.parse(row2.valueMap(colName)).toOption.get.location
        location1.compare(location2)
      }
    }
    val rowsSorted = TreeSet[TsvRow]()(orderingByIds) ++ reader
    val writer = TsvWriter(outFile, reader.header, resourceConfig)
    rowsSorted.foreach(writer.addRow)
  }

  def extractUniqueValues(inFile: InputId, outFile: OutputId, readerGenerator: InputId => BasicTsvReader,
                          colName: String): Unit = {
    val reader = readerGenerator(inFile)
    val colIndex = reader.cols.indexOf(colName)
    if (colIndex < 0) {
      throw new Exception(s"File $inFile does not have a column $colName.")
    }
    inFile match {
      case FileInputId(inFileFile) =>
        outFile match {
          case FileOutputId(outFileFile) =>
            val nHeaderLines = reader.header.lines.size
            val commandString =
              s"{ echo $colName; tail -n +${nHeaderLines + 1} $inFileFile | cut -f ${colIndex + 1} | sort -u ; } > $outFileFile"
            println(commandString)
            ExecutionUtils.runBashScript(commandString)
          case _ => throw new Exception(s"Output file for this feature needs to be local file, but $outFile is not.")
        }
      case _ => throw new Exception(s"Input file for this feature needs to be local file, but $inFile is not.")
    }
  }

  def loadVariantGroupIds(file: InputId, idCol: String, resourceConfig: ResourceConfig): Set[VariantGroupId] = {
    val readerSelection = BasicTsvReader.forSimpleHeaderLine(file, resourceConfig)
    val selectedIdStrings = readerSelection.flatMap(_.valueMap.get(idCol))
    selectedIdStrings.map(VariantGroupId.parse).collect {
      case Right(id) => id
    }.toSet
  }
}
