package chowser.cmd

import better.files.File
import chowser.genomics.Region

case class VariantsForRegionCommand(inFile: File, outFile: File, chromColName: String, posColName: String,
                                    region: Region)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
