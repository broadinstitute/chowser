package chowser.genomics

import htsjdk.variant.variantcontext.VariantContext

case class VariantIdNew(location: Location, ref: String, alt: String) extends VariantId {

  def fromVariantContext(context: VariantContext): Either[String, VariantIdNew] = {
    Chromosome.parse(context.getContig) match {
      case Left(message) => Left(message)
      case Right(chromosome) =>
        val pos = context.getStart
        val ref = context.getReference.getBaseString
        ??? // TODO
    }
  }

}
