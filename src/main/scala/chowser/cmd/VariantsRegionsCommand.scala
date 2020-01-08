package chowser.cmd

import chowser.util.io.IoId

case class VariantsRegionsCommand(inFile: IoId, outFile: IoId, chromColName: String, posColName: String, radius: Int)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
