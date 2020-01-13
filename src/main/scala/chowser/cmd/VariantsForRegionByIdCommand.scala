package chowser.cmd

import chowser.genomics.Region
import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsForRegionByIdCommand(resourceConfig: ResourceConfig, inFile: InputId, outFile: OutputId,
                                        idColName: String, region: Region)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
