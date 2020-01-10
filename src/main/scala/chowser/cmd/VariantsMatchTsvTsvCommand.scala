package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class VariantsMatchTsvTsvCommand(tsv1: InputId, tsv2: InputId, idCol1: String, idCol2: String,
                                      inBothOpt: Option[OutputId], onlyInOneOpt: Option[OutputId],
                                      onlyInTwoOpt: Option[OutputId], keyFileOpt: Option[InputId])
  extends ChowserCommand {

}
