package chowser.cmd

import better.files.File

case class VariantsForRegionCommand(inFile: File, outFile: File, chromColName: String, posColName: String,
                                    chromosome: String, start: Int, end: Int)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
