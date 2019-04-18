package chowser.util.genomics

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
    if(context.emptyID()) {
      Left("No id given")
    } else {
      val id = context.getID()
      if(id.isEmpty || id == ".") {
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

}
