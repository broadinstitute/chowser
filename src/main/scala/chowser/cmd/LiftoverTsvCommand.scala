package chowser.cmd

import better.files.File
import chowser.cmd.LiftoverTsvCommand.ChromPosCols

case class LiftoverTsvCommand(inFile: File, chainFile: File, outFile: File,
                              idColOpt: Option[String], chromPosColsOpt: Option[ChromPosCols])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

object LiftoverTsvCommand {
  case class ChromPosCols(chromCol: String, posCol: String)
}
