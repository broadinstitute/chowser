package chowser.cmd

import chowser.cmd.LiftoverTsvCommand.ChromPosCols
import chowser.util.io.{InputId, OutputId}

case class LiftoverTsvCommand(inFile: InputId, chainFile: InputId, outFile: OutputId,
                              idColOpt: Option[String], chromPosColsOpt: Option[ChromPosCols],
                              keyFileOpt: Option[InputId])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

object LiftoverTsvCommand {
  case class ChromPosCols(chromCol: String, posCol: String)
}
