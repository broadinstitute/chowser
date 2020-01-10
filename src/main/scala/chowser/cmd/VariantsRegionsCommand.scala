package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class VariantsRegionsCommand(inFile: InputId, outFile: OutputId, chromColName: String, posColName: String,
                                  radius: Int, keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}
