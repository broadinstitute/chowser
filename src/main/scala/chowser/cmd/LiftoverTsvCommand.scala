package chowser.cmd

import chowser.cmd.LiftoverTsvCommand.ChromPosCols
import chowser.util.io.IoId

case class LiftoverTsvCommand(inFile: IoId, chainFile: IoId, outFile: IoId,
                              idColOpt: Option[String], chromPosColsOpt: Option[ChromPosCols])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

object LiftoverTsvCommand {
  case class ChromPosCols(chromCol: String, posCol: String)
}
