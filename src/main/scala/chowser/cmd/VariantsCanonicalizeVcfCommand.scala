package chowser.cmd

import chowser.util.io.IoId

case class VariantsCanonicalizeVcfCommand(inFile: IoId, outFile: IoId)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

