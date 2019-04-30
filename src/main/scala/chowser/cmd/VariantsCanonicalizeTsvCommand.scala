package chowser.cmd

import better.files.File

case class VariantsCanonicalizeTsvCommand(inFile: File, outFile: File,
                                          idCol: String, chromosomeCol: String, positionCol: String, refCol: String,
                                          altCol: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

