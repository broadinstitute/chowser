package chowser.execute

import chowser.cmd.VariantsCanonicalizeVcfCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.VariantGroupId
import chowser.vcf.HtsjdkUtils
import htsjdk.variant.variantcontext.VariantContextBuilder
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsCanonicalizeVcfByHtsjdkExecuter extends ChowserExecuter[VariantsCanonicalizeVcfCommand] {

  override def execute(command: VariantsCanonicalizeVcfCommand): Either[Snag, Result] = {
    import command.{inFile, outFile}
    HtsjdkUtils.transformVcf(inFile.file, outFile.file) { variantContextIter =>
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
