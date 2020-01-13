package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

case class VariantsMatchVcfTsvCommand(resourceConfig: ResourceConfig, vcf: InputId, tsv: InputId, idCol: String,
                                      inBothOpt: Option[OutputId], vcfOnlyOpt: Option[OutputId],
                                      tsvOnlyOpt: Option[OutputId])
  extends ChowserCommand {

}
