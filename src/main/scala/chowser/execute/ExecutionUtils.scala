package chowser.execute

import better.files.File
import chowser.filter.Filter
import chowser.tsv.TsvReader
import chowser.tsv.TsvReader.Row

object ExecutionUtils {

  def filterRows(inFile: File, outFile: File, readerGenerator: File => TsvReader, filter: Filter[Row]): Unit = {
    val reader = readerGenerator(inFile)
    if(outFile.nonEmpty) {
      outFile.clear()
    }
    reader.headerLines.foreach(outFile.appendLine(_))
    reader.filter(filter).map(_.line).foreach(outFile.appendLine(_))
  }

}
