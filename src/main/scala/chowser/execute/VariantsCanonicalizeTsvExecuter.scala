package chowser.execute

import chowser.cmd.VariantsCanonicalizeTsvCommand
import chowser.execute.ChowserExecuter.Result
import chowser.genomics.{Chromosome, VariantGroupId}
import chowser.tsv.{BasicTsvReader, TsvRow, TsvWriter}
import chowser.util.NumberParser
import org.broadinstitute.yootilz.core.snag.Snag

object VariantsCanonicalizeTsvExecuter extends ChowserExecuter[VariantsCanonicalizeTsvCommand] {

  override def execute(command: VariantsCanonicalizeTsvCommand): Either[Snag, Result] = {
    import command._
    val reader = BasicTsvReader.forSimpleHeaderLine(inFile, resourceConfig)
    val writer = TsvWriter(outFile, reader.header, resourceConfig)
    reader.map(updateWithCanonicalId(idCol, chromosomeCol, positionCol, refCol, altCol, _ => ()))
      .foreach(writer.addRow)
    Right(Result.Done)
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
}
