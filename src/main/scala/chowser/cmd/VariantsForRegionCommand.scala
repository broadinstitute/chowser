package chowser.cmd

import chowser.genomics.Region
import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsForRegionCommand(resourceConfig: ResourceConfig, inFile: InputId, outFile: OutputId,
                                    chromColName: String, posColName: String, region: Region)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
