package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class TsvMatrixCommand(valuesFile: InputId, idsFile: InputId, outFile: OutputId,
                            idCol1: String, idCol2: String, valueCol: String, keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneOutFile {
}

