package chowser.cmd

import chowser.genomics.Region
import chowser.util.io.{InputId, OutputId}

case class VariantsForRegionCommand(inFile: InputId, outFile: OutputId, chromColName: String, posColName: String,
                                    region: Region, keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
