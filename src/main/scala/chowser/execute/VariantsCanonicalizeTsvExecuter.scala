package chowser.execute

import chowser.cmd.VariantsCanonicalizeTsvCommand
import chowser.genomics.{Chromosome, VariantGroupId}
import chowser.tsv.{BasicTsvReader, TsvRow}
import chowser.util.NumberParser

object VariantsCanonicalizeTsvExecuter extends ChowserExecuter[VariantsCanonicalizeTsvCommand] {

  def execute(command: VariantsCanonicalizeTsvCommand): Result = {
    import command._
    val reader = BasicTsvReader.forSimpleHeaderLine(inFile)
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    reader.header.lines.foreach(outFile.appendLine(_))
    reader.map(updateWithCanonicalId(idCol, chromosomeCol, positionCol, refCol, altCol, _ => ()))
      .map(_.line).foreach(outFile.appendLine(_))
    Result(command, success = true)
  }

  def updateWithCanonicalId(idCol: String, chromosomeCol: String,
                            positionCol: String, refCol: String, altCol: String,
                            reporter: String => Unit)(row: TsvRow): TsvRow = {
    createCanonicalId(row.valueMap, idCol, chromosomeCol, positionCol, refCol, altCol) match {
      case Left(message) =>
        reporter(message)
        row
      case Right(variantGroupId) =>
        row.withValue(idCol, variantGroupId.toString)
    }
  }

  def createCanonicalId(values: Map[String, String], idCol: String, chromosomeCol: String,
                        positionCol: String, refCol: String, altCol: String): Either[String, VariantGroupId] = {
    values.get(chromosomeCol) match {
      case Some(chromosomeString) =>
        Chromosome.parse(chromosomeString) match {
          case Left(message) => Left(message)
          case Right(chromosome) =>
            values.get(positionCol) match {
              case Some(positionString) =>
                NumberParser.UnsignedIntParser.parseOpt(positionString) match {
                  case Some(position) =>
                    values.get(refCol) match {
                      case Some(ref) =>
                        values.get(altCol) match {
                          case Some(altsString) =>
                            val alts = VariantGroupId.parseAlts(altsString)
                            Right(VariantGroupId(chromosome, position, ref, alts))
                          case None => Left("No alt")
                        }
                      case None => Left("No ref")
                    }
                  case None => Left("Invalid position")
                }
              case None => Left("No position")
            }
        }
      case None => Left("No chromosome")
    }
  }

  case class Result(command: VariantsCanonicalizeTsvCommand, success: Boolean)
    extends ChowserExecuter.Result[VariantsCanonicalizeTsvCommand]

}
