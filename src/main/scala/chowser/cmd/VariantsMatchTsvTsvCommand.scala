package chowser.cmd

import chowser.util.io.IoId

case class VariantsMatchTsvTsvCommand(tsv1: IoId, tsv2: IoId, idCol1: String, idCol2: String,
                                      inBothOpt: Option[IoId], onlyInOneOpt: Option[IoId], onlyInTwoOpt: Option[IoId])
  extends ChowserCommand {

}
