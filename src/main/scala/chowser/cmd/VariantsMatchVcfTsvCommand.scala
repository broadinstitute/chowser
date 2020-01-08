package chowser.cmd

import chowser.util.io.IoId

case class VariantsMatchVcfTsvCommand(vcf: IoId, tsv: IoId, idCol: String,
                                      inBothOpt: Option[IoId], vcfOnlyOpt: Option[IoId], tsvOnlyOpt: Option[IoId])
  extends ChowserCommand {

}
