package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class VariantsCanonicalizeVcfCommand(inFile: InputId, outFile: OutputId, keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

