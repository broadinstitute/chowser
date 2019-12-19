package chowser.vcf

import better.files.File
import chowser.genomics.Location
import htsjdk.samtools.liftover.LiftOver
import htsjdk.samtools.util.Interval
import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.variantcontext.writer.VariantContextWriterBuilder
import htsjdk.variant.vcf.VCFFileReader

import scala.collection.JavaConverters.asScalaIteratorConverter

object HtsjdkUtils {
  def transformVcf(inFile: File, outFile: File)
                  (transformation: Iterator[VariantContext] => Iterator[VariantContext]): Unit = {
    val reader = new VCFFileReader(inFile.path, false)
    val header = reader.getFileHeader
    val dict = header.getSequenceDictionary
    val writer = new VariantContextWriterBuilder().setOutputPath(outFile.path).setReferenceDictionary(dict).build
    writer.writeHeader(header)
    val variantContextIter = reader.iterator().asScala
    transformation(variantContextIter).foreach(writer.add)
    writer.close()
  }

  def liftOverKeepChromosome(liftOver: LiftOver, location: Location): Either[String, Location] = {
    val intervalOriginal = new Interval(location.chromosome.toString, location.position, location.position)
    val intervalLiftedOver = liftOver.liftOver(intervalOriginal)
    if (intervalLiftedOver == null) {
      Left(s"Location $location cannot be lifted over.")
    } else {
      Right(Location(location.chromosome, intervalLiftedOver.getStart))
    }
  }
}
