package chowser.cmd

import chowser.genomics.Region
import chowser.util.io.{InputId, OutputId}

case class VariantsForRegionByIdCommand(inFile: InputId, outFile: OutputId, idColName: String, region: Region)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
