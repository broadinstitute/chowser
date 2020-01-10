package chowser.cmd

import chowser.filter.Filter
import chowser.util.io.{InputId, OutputId}

case class TsvSliceCommand(inFile: InputId, outFile: OutputId, colName: String, filter: Filter[String],
                           keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

