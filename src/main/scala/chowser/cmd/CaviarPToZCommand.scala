package chowser.cmd

import better.files.File

case class CaviarPToZCommand(inFile: File, outFile: File, idCol: String, pCol: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

