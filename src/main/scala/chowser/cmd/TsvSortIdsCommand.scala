package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class TsvSortIdsCommand(inFile: InputId, outFile: OutputId, colName: String, keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

