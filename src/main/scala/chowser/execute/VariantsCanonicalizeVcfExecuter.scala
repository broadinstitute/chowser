package chowser.execute

import chowser.cmd.VariantsCanonicalizeVcfCommand
import chowser.vcf.VcfUtils

object VariantsCanonicalizeVcfExecuter extends ChowserExecuter[VariantsCanonicalizeVcfCommand] {

  def execute(command: VariantsCanonicalizeVcfCommand): Result = {
    import command.{inFile, outFile}
    VcfUtils.transformVcf(inFile, outFile) { vcfRecordIter =>
      vcfRecordIter.flatMap { vcfRecord =>
        vcfRecord.withCanonicalId match {
          case Left(message) =>
            println(message)
            Seq.empty
          case Right(vcfRecordCanonicalized) =>
            Seq(vcfRecordCanonicalized)
        }
      }
    }
    Result(command, success = true)
  }

  case class Result(command: VariantsCanonicalizeVcfCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsCanonicalizeVcfCommand]

}
