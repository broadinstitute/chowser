package chowser.cmd

import chowser.util.io.IoId

case class TsvMatrixCommand(valuesFile: IoId, idsFile: IoId, outFile: IoId,
                            idCol1: String, idCol2: String, valueCol: String)
  extends ChowserCommand with ChowserCommand.OneOutFile {
}

