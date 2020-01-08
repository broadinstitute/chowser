package chowser.cmd

import chowser.util.io.IoId

case class VariantsCanonicalizeTsvCommand(inFile: IoId, outFile: IoId,
                                          idCol: String, chromosomeCol: String, positionCol: String, refCol: String,
                                          altCol: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

