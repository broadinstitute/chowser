package chowser.cmd

import better.files.File

case class CompareVariantsCommand(vcf: File, tsv: File, idCol: String, chromCol: String, posCol: String,
                             inBothOpt: Option[File], vcfOnlyOpt: Option[File], tsvOnlyOpt: Option[File])
  extends ChowserCommand {

}
