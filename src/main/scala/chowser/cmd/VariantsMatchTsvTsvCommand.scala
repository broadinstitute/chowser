package chowser.cmd

import better.files.File

case class VariantsMatchTsvTsvCommand(tsv1: File, tsv2: File, idCol1: String, idCol2: String,
                                      inBothOpt: Option[File], onlyInOneOpt: Option[File], onlyInTwoOpt: Option[File])
  extends ChowserCommand {

}
