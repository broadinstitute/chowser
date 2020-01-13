package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsCanonicalizeTsvCommand(resourceConfig: ResourceConfig, inFile: InputId, outFile: OutputId,
                                          idCol: String, chromosomeCol: String, positionCol: String, refCol: String,
                                          altCol: String)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

