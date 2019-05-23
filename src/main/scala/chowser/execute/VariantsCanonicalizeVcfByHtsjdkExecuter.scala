package chowser.execute

import chowser.cmd.VariantsCanonicalizeVcfCommand
import chowser.genomics.VariantGroupId
import chowser.vcf.HtsjdkUtils
import htsjdk.variant.variantcontext.VariantContextBuilder

object VariantsCanonicalizeVcfByHtsjdkExecuter extends ChowserExecuter[VariantsCanonicalizeVcfCommand] {

  def execute(command: VariantsCanonicalizeVcfCommand): Result = {
    import command.{inFile, outFile}
    HtsjdkUtils.transformVcf(inFile, outFile) { variantContextIter =>
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
    Result(command, success = true)
  }

  case class Result(command: VariantsCanonicalizeVcfCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsCanonicalizeVcfCommand]

}
