package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class VariantsCanonicalizeTsvCommand(inFile: InputId, outFile: OutputId,
                                          idCol: String, chromosomeCol: String, positionCol: String, refCol: String,
                                          altCol: String, keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

