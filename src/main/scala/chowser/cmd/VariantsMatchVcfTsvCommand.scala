package chowser.cmd

import better.files.File

case class VariantsMatchVcfTsvCommand(vcf: File, tsv: File, idCol: String,
                                      inBothOpt: Option[File], vcfOnlyOpt: Option[File], tsvOnlyOpt: Option[File])
  extends ChowserCommand {

}
