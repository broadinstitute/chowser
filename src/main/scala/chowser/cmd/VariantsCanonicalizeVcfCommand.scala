package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsCanonicalizeVcfCommand(resourceConfig: ResourceConfig, inFile: InputId, outFile: OutputId)
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

