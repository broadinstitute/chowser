package chowser.cmd

import better.files.File

case class VariantsRegionsCommand(inFile: File, outFile: File, chromColName: String, posColName: String, radius: Int)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
