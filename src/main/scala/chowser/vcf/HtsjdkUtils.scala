package chowser.vcf

import better.files.File
import chowser.genomics.{Chromosome, Location}
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

  def liftOver(htsjdkLiftOver: LiftOver, location: Location, chromToString: Chromosome => String):
  Either[String, Location] = {
    val chromosomeString = chromToString(location.chromosome)
    val position = location.position
    val intervalOriginal = new Interval(chromosomeString, position, position)
    val intervalLiftedOver = htsjdkLiftOver.liftOver(intervalOriginal)
    if(intervalLiftedOver == null) {
      Left(s"Cannot map $chromosomeString:$position")
    } else {
      Chromosome.parse(intervalLiftedOver.getContig) match {
        case Left(message) => Left(message)
        case Right(chromosome) => Right(Location(chromosome, intervalLiftedOver.getStart))
      }
    }
  }

  def liftOver(htsjdkLiftOver: LiftOver, location: Location): Either[String, Location] = {
    liftOver(htsjdkLiftOver, location, _.inEnsembleNotation) match {
      case Left(message1) =>
        liftOver(htsjdkLiftOver, location, _.inUcscNotation) match {
          case Left(message2) => Left(message1 +"; " + message2)
          case Right(location) => Right(location)
        }
      case Right(location) => Right(location)
    }
  }
}
