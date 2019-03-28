package chowser.cmd

import better.files.File
import chowser.filter.Filter

case class TsvFilterCommand(inFile: File, outFile: File, colName: String, filter: Filter[Double])
  extends ChowserCommand {

}

object TsvFilterCommand {

}
