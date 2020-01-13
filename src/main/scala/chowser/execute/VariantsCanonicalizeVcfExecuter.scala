package chowser.execute

import chowser.cmd.VariantsCanonicalizeVcfCommand
import chowser.execute.ChowserExecuter.Result
import chowser.util.io.IoIdUtils
import chowser.vcf.VcfUtils
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsCanonicalizeVcfExecuter extends ChowserExecuter[VariantsCanonicalizeVcfCommand] {

  override def execute(command: VariantsCanonicalizeVcfCommand): Either[Snag, Result] = {
    import command.{inFile, outFile, resourceConfig}
    IoIdUtils.needsToBeFile(inFile) { inFileFile =>
      IoIdUtils.needsToBeFile(outFile) { outFileFile =>
        VcfUtils.transformVcf(inFileFile, outFileFile, resourceConfig) { vcfRecordIter =>
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
        Right(Result.Done)
      }
    }
  }
}
