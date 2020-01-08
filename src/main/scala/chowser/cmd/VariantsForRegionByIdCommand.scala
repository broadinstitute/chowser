package chowser.cmd

import chowser.genomics.Region
import chowser.util.io.IoId

case class VariantsForRegionByIdCommand(inFile: IoId, outFile: IoId, idColName: String, region: Region)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
