package chowser.execute

import chowser.cmd.VariantsCanonicalizeTsvCommand
import chowser.genomics.{Chromosome, VariantId}
import chowser.tsv.TsvReader
import chowser.util.NumberParser

object VariantsCanonicalizeTsvExecuter extends ChowserExecuter[VariantsCanonicalizeTsvCommand] {

  def execute(command: VariantsCanonicalizeTsvCommand): Result = {
    import command.{inFile, outFile, idCol, chromosomeCol, positionCol, refCol, altCol}
    val reader = TsvReader.forSimpleHeaderLine(inFile)
    if (outFile.nonEmpty) {
      outFile.clear()
    }
    println("yo!")
    reader.headerLines.foreach(outFile.appendLine(_))
    reader.map(updateWithCanonicalId(idCol, chromosomeCol, positionCol, refCol, altCol, println))
      .map(_.line).foreach(outFile.appendLine(_))
    Result(command, success = true)
  }

  def updateWithCanonicalId(idCol: String, chromosomeCol: String,
                            positionCol: String, refCol: String, altCol: String,
                            reporter: String => Unit)(row: TsvReader.Row): TsvReader.Row = {
    createCanonicalId(row.valueMap, idCol, chromosomeCol, positionCol, refCol, altCol) match {
      case Left(message) =>
        reporter(message)
        println(message)
        row
      case Right(variantId) =>
        println(variantId)
        row.withValue(idCol, variantId.toString)
    }
  }

  def createCanonicalId(values: Map[String, String], idCol: String, chromosomeCol: String,
                        positionCol: String, refCol: String, altCol: String): Either[String, VariantId] = {
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
                          case Some(alt) => Right(VariantId(chromosome, position, ref, alt))
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
