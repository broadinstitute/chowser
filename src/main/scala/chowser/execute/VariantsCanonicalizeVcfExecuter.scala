package chowser.execute

import chowser.cmd.VariantsCanonicalizeVcfCommand
import chowser.genomics.VariantGroupId
import htsjdk.variant.variantcontext.VariantContextBuilder
import htsjdk.variant.variantcontext.writer.VariantContextWriterBuilder
import htsjdk.variant.vcf.VCFFileReader

import scala.collection.JavaConverters.asScalaIteratorConverter

object VariantsCanonicalizeVcfExecuter extends ChowserExecuter[VariantsCanonicalizeVcfCommand] {

  def execute(command: VariantsCanonicalizeVcfCommand): Result = {
    import command.{inFile, outFile}
    val reader = new VCFFileReader(inFile.path, false)
    val header = reader.getFileHeader
    val dict = header.getSequenceDictionary
    val writer = new VariantContextWriterBuilder().setOutputPath(outFile.path).setReferenceDictionary(dict).build
    writer.writeHeader(header)
    val variantContextIter = reader.iterator().asScala
    variantContextIter.foreach { context =>
      VariantGroupId.fromVariantContext(context) match {
        case Right(newId) =>
          val contextNew = new VariantContextBuilder(context).id(newId.toString).make
          writer.add(contextNew)
        case Left(message) => println(message)
      }
    }
    writer.close()
    Result(command, success = true)
  }

  case class Result(command: VariantsCanonicalizeVcfCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsCanonicalizeVcfCommand]

}
