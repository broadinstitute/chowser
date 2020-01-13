package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsMatchTsvTsvCommand(resourceConfig: ResourceConfig, tsv1: InputId, tsv2: InputId,
                                      idCol1: String, idCol2: String,
                                      inBothOpt: Option[OutputId], onlyInOneOpt: Option[OutputId],
                                      onlyInTwoOpt: Option[OutputId])
  extends ChowserCommand {

}
