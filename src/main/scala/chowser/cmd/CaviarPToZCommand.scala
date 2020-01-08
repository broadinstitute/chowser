package chowser.cmd

import chowser.util.io.IoId

case class CaviarPToZCommand(inFile: IoId, outFile: IoId, idCol: String, pCol: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

