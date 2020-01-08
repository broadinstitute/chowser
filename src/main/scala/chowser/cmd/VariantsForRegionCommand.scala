package chowser.cmd

import chowser.genomics.Region
import chowser.util.io.IoId

case class VariantsForRegionCommand(inFile: IoId, outFile: IoId, chromColName: String, posColName: String,
                                    region: Region)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
