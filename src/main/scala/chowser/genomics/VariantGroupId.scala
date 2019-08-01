package chowser.genomics

import better.files.File
import chowser.genomics.VariantId.coordinateDividerRegex
import chowser.tsv.{BasicTsvReader, TsvWriter}
import chowser.util.NumberParser
import htsjdk.variant.variantcontext.VariantContext

import scala.collection.JavaConverters.asScalaBufferConverter

case class VariantGroupId(location: Location, ref: String, alts: Seq[String]) extends Location.HasLocation {

  def alleles: Seq[String] = ref +: alts

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

  def parseAlts(altsString: String): Seq[String] = {
    altsString.split(VariantId.altsDividerRegex).map(VariantId.adjustSequenceIfEmpty).toSeq
  }

  def parse(string: String): Either[String, VariantGroupId] = {
    val parts = string.split(coordinateDividerRegex)
    if(parts.size < 4 || parts.size > 5) {
      Left(s"""Unrecognized format of variant id \"$string\"""")
    } else {
      parse(parts(0), parts(1), parts(2), parts(3))
    }
  }

  def parse(chromString: String, posString: String, refString: String,
            altString: String): Either[String, VariantGroupId] = {
    Chromosome.parse(chromString) match {
      case Left(message) => Left(message)
      case Right(chromosome) =>
        NumberParser.UnsignedIntParser.parseOpt(posString) match {
          case None => Left(s"""Invalid format for position \"$posString\"""")
          case Some(pos) =>
            val ref = VariantId.adjustSequenceIfEmpty(refString)
            val alts = parseAlts(altString)
            Right(VariantGroupId(chromosome, pos, ref, alts))
        }
    }
  }

  class VariantGroupIdTsvReader(val idKey: String)(val file: File) extends Iterator[VariantGroupId] {
    val tsvReader: BasicTsvReader = BasicTsvReader.forSimpleHeaderLine(file)

    val delegate: Iterator[VariantGroupId] =
      tsvReader.map(_.valueMap).map(_.get(idKey)).collect {
        case Some(id) => id
      }.map(parse).collect {
        case Right(variantId) => variantId
      }

    override def hasNext: Boolean = delegate.hasNext

    override def next(): VariantGroupId = delegate.next()
  }

  class VariantGroupIdTsvWriter(val idKey: String)(val file: File) {
    val tsvWriter = new TsvWriter(file, Seq(idKey))

    def add(variantGroupId: VariantGroupId): Unit = {
      tsvWriter.addRow(Map(idKey -> variantGroupId.toString))
    }
  }



}
