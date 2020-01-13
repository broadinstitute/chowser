package chowser.execute

import chowser.cmd.VariantsCanonicalizeVcfCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.util.io.{FileInputId, FileOutputId, IoIdUtils}
import chowser.vcf.HtsjdkUtils
import htsjdk.variant.variantcontext.VariantContextBuilder
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsCanonicalizeVcfByHtsjdkExecuter extends ChowserExecuter[VariantsCanonicalizeVcfCommand] {

  override def execute(command: VariantsCanonicalizeVcfCommand): Either[Snag, Result] = {
    import command.{inFile, outFile}
    IoIdUtils.needsToBeFile(inFile) { inFileFile =>
      IoIdUtils.needsToBeFile(outFile) { outFileFile =>
        HtsjdkUtils.transformVcf(inFileFile.file, outFileFile.file) { variantContextIter =>
          variantContextIter.flatMap { context =>
            VariantGroupId.fromVariantContext(context) match {
              case Right(newId) =>
                Some(new VariantContextBuilder(context).id(newId.toString).make)
              case Left(message) =>
                println(message)
                None
            }
          }
        }
        Right(Result.Done)
      }
    }
  }
}
