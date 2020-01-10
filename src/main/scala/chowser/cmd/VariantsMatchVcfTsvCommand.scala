package chowser.cmd

import chowser.util.io.{InputId, OutputId}

case class VariantsMatchVcfTsvCommand(vcf: InputId, tsv: InputId, idCol: String,
                                      inBothOpt: Option[OutputId], vcfOnlyOpt: Option[OutputId],
                                      tsvOnlyOpt: Option[OutputId], keyFileOpt: Option[InputId])
  extends ChowserCommand {

}
