package chowser.cmd

import better.files.File

case class LiftoverTsvCommand(inFile: File, chainFile: File, outFile: File,
                              idColOpt: Option[String], posColOpt: Option[String])
  extends ChowserCommand with ChowserCommand.OneInFile with ChowserCommand.OneOutFile {

}

