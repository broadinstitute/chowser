package chowser.genomics

import chowser.genomics.VariantId.coordinateDividerRegex
import chowser.util.NumberParser
import htsjdk.variant.variantcontext.VariantContext

import scala.collection.JavaConverters.asScalaBufferConverter

case class VariantGroupId(location: Location, ref: String, alts: Seq[String]) extends Location.HasLocation {

  def variants: Seq[VariantId] = {
    alts.map(alt => VariantId(chromosome, position, ref, alt))
  }

  override def toString: String = {
    val altsString = alts.mkString(",")
    Seq(chromosome, position, ref, altsString).mkString("_")
  }

}

object VariantGroupId {
  def apply(chromosome: Chromosome, position: Int, ref: String, alts: Seq[String]): VariantGroupId =
    VariantGroupId(Location(chromosome, position), ref, alts)

  def fromVariantContext(context: VariantContext): Either[String, VariantGroupId] = {
    Chromosome.parse(context.getContig) match {
      case Left(message) => Left(message)
      case Right(chromosome) =>
        val pos = context.getStart
        val ref = VariantId.adjustSequenceIfEmpty(context.getReference.getBaseString)
        val alts = context.getAlternateAlleles.asScala.map(_.getBaseString).map(VariantId.adjustSequenceIfEmpty)
          Right(VariantGroupId(chromosome, pos, ref, alts))
    }
  }

  def parse(string: String): Either[String, VariantGroupId] = {
    val parts = string.split(coordinateDividerRegex)
    if(parts.size < 4 || parts.size > 5) {
      Left(s"""Unrecognized format of variant id \"$string\"""")
    } else {
      Chromosome.parse(parts(0)) match {
        case Left(message) => Left(message)
        case Right(chromosome) =>
          val posString = parts(1)
          NumberParser.UnsignedIntParser.parseOpt(posString) match {
            case None => Left(s"""Invalid format for position \"$posString\"""")
            case Some(pos) =>
              val ref = VariantId.adjustSequenceIfEmpty(parts(2))
              val alts = parts(3).split(VariantId.altsDividerRegex).map(VariantId.adjustSequenceIfEmpty).toSeq
              Right(VariantGroupId(chromosome, pos, ref, alts))
          }
      }
    }
  }

}
