package chowser.execute

import chowser.cmd.TsvFilterCommand
import chowser.filter.{RowFilters, StringFilters}
import chowser.tsv.TsvReader

object TsvFilterExecuter {

  def execute(command: TsvFilterCommand): ChowserExecuter.Result = {
    import command.{inFile, outFile, colName, filter}
    val rowIterator = TsvReader.forSimpleHeaderLine(inFile)
    val rowFilter = RowFilters.ForCol(colName, StringFilters.parsesAsDoubleAndFilter(filter))
    if(outFile.nonEmpty) {
      outFile.clear()
    }
    rowIterator.headers.foreach(outFile.appendLine(_))
    rowIterator.filter(rowFilter).map(_.line).foreach(outFile.appendLine(_))
    Result(command, success = true)
  }

  case class Result(command: TsvFilterCommand, success: Boolean) extends ChowserExecuter.Result

}
