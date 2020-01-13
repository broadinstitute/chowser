package chowser.cmd

import chowser.cmd.LiftoverTsvCommand.ChromPosCols
import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class LiftoverTsvCommand(resourceConfig: ResourceConfig, inFile: InputId, chainFile: InputId, outFile: OutputId,
                              idColOpt: Option[String], chromPosColsOpt: Option[ChromPosCols])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

object LiftoverTsvCommand {
  case class ChromPosCols(chromCol: String, posCol: String)
}
