package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class CaviarPToZCommand(inFile: InputId, outFile: OutputId, idCol: String, pCol: String,
                             keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

