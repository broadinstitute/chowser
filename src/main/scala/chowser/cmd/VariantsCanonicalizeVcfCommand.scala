package chowser.cmd

import better.files.File

case class VariantsCanonicalizeVcfCommand(inFile: File, outFile: File)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

