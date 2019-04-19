package chowser.util.genomics

import better.files.File
import chowser.tsv.{TsvReader, TsvWriter}
import chowser.util.NumberParser
import htsjdk.variant.variantcontext.VariantContext

case class VariantIdLocation(id: String, location: Location) {

}

object VariantIdLocation {

  def fromMap(idKey: String, chromosomeKey: String,
              positionKey: String)(values: Map[String, String]): Either[String, VariantIdLocation] = {
    (values.get(idKey), values.get(chromosomeKey), values.get(positionKey)) match {
      case (None, _, _) => Left("No variant id")
      case (_, None, _) => Left("No chromosome given")
      case (_, _, None) => Left("No position given")
      case (Some(id), Some(chrString), Some(posString)) =>
        if (id.isEmpty || id == ".") {
          Left("No id given")
        } else {
          Chromosome.parse(chrString) match {
            case Left(message) => Left(message)
            case Right(chromosome) =>
              NumberParser.UnsignedIntParser.parseOpt(posString) match {
                case None => Left(s"$posString is not a valid position.")
                case Some(position) =>
                  Right(VariantIdLocation(id, Location(chromosome, position)))
              }
          }
        }
    }
  }

  def fromVariantContext(context: VariantContext): Either[String, VariantIdLocation] = {
    if (context.emptyID()) {
      Left("No id given")
    } else {
      val id = context.getID()
      if (id.isEmpty || id == ".") {
        Left("No id given")
      } else {
        val chromString = ???
        Chromosome.parse(chromString) match {
          case Left(message) => Left(message)
          case Right(chromosome) =>
            val pos = context.getStart
            Right(VariantIdLocation(id, Location(chromosome, pos)))
        }
      }
    }
  }

  class VariantIdLocationTsvReader(val idKey: String, val chromosomeKey: String,
                                   val positionKey: String)(val file: File) extends Iterator[VariantIdLocation] {
    val tsvReader = TsvReader.forSimpleHeaderLine(file)

    val delegate = tsvReader.map(_.valueMap).map(fromMap(idKey, chromosomeKey, positionKey)(_)).collect {
      case Right(variantIdLocation) => variantIdLocation
    }

    override def hasNext: Boolean = delegate.hasNext

    override def next(): VariantIdLocation = delegate.next()
  }

  class VariantIdLocationTsvWriter(val idKey: String, val chromosomeKey: String,
                                   val positionKey: String)(val file: File) {
    val tsvWriter = new TsvWriter(file, Seq(idKey, chromosomeKey, positionKey))

    def add(variantIdLocation: VariantIdLocation): Unit = {
      tsvWriter.addRow(
        Map(
          idKey -> variantIdLocation.id,
          chromosomeKey -> variantIdLocation.location.chromosome.inEnsembleNotation,
          positionKey -> variantIdLocation.location.position.toString
        )
      )
    }
  }

}
