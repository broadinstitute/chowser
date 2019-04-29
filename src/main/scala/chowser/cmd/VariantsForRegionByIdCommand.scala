package chowser.cmd

import better.files.File
import chowser.genomics.Region

case class VariantsForRegionByIdCommand(inFile: File, outFile: File, idColName: String, region: Region)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
