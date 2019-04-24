package chowser.genomics

import better.files.File
import chowser.tsv.{TsvReader, TsvWriter}
import chowser.util.NumberParser
import htsjdk.variant.variantcontext.VariantContext

import scala.collection.JavaConverters.asScalaBufferConverter

case class VariantId(location: Location, ref: String, alt: String) {

  override def toString: String = Seq(location.chromosome, location.position, ref, alt).map(_.toString).mkString("_")

}

object VariantId {

  def apply(chromosome: Chromosome, position: Int, ref: String, alt: String): VariantId =
    VariantId(Location(chromosome, position), ref, alt)

  val emptySequenceString: String = "-"
  def adjustSequenceIfEmpty(sequence: String): String = {
    if(sequence.isEmpty) emptySequenceString else sequence
  }

  def fromVariantContext(context: VariantContext): Seq[Either[String, VariantId]] = {
    Chromosome.parse(context.getContig) match {
      case Left(message) => Seq(Left(message))
      case Right(chromosome) =>
        val pos = context.getStart
        val ref = VariantId.adjustSequenceIfEmpty(context.getReference.getBaseString)
        val alts = context.getAlternateAlleles.asScala.map(_.getBaseString).map(VariantId.adjustSequenceIfEmpty)
        alts.map { alt =>
          Right(VariantId(chromosome, pos, ref, alt))
        }
    }
  }

  val dividerRegex: String = "[^a-zA-Z0-9-]"

  def parse(string: String): Either[String, VariantId] = {
    val parts = string.split(dividerRegex)
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
              val alt = VariantId.adjustSequenceIfEmpty(parts(3))
              Right(VariantId(chromosome, pos, ref, alt))
          }
      }
    }
  }

  class VariantIdTsvReader(val idKey: String)(val file: File) extends Iterator[VariantId] {
    val tsvReader: TsvReader = TsvReader.forSimpleHeaderLine(file)

    val delegate: Iterator[VariantId] =
      tsvReader.map(_.valueMap).map(_.get(idKey)).collect {
        case Some(id) => id
      }.map(parse).collect {
        case Right(variantId) => variantId
      }

    override def hasNext: Boolean = delegate.hasNext

    override def next(): VariantId = delegate.next()
  }

  class VariantIdTsvWriter(val idKey: String)(val file: File) {
    val tsvWriter = new TsvWriter(file, Seq(idKey))

    def add(variantId: VariantId): Unit = {
      tsvWriter.addRow(Map(idKey -> variantId.toString))
    }
  }


}
