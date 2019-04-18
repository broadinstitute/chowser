package chowser.util.genomics

import chowser.util.NumberParser

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

}
