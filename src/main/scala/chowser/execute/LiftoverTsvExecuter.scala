package chowser.execute

import chowser.cmd.LiftoverTsvCommand
import chowser.cmd.LiftoverTsvCommand.ChromPosCols
import chowser.genomics.{Chromosome, Location, VariantGroupId}
import chowser.tsv.{BasicTsvReader, TsvRow, TsvWriter}
import chowser.util.NumberParser
import chowser.vcf.HtsjdkUtils
import htsjdk.samtools.liftover.LiftOver

object LiftoverTsvExecuter extends ChowserExecuter[LiftoverTsvCommand] {

  def execute(command: LiftoverTsvCommand): Result = {
    import command.{chainFile, inFile, outFile, idColOpt, chromPosColsOpt}
    val liftOver = new LiftOver(chainFile.toJava)
    val reader = BasicTsvReader.forSimpleHeaderLine(inFile)
    val writer = TsvWriter(outFile, reader.headerLines)
    val rowMapper: TsvRow => Iterator[TsvRow] = { row =>
      val rowNewOpt =
        idColOpt match {
          case Some(idCol) =>
            row.valueMap.get(idCol) match {
              case Some(idString) =>
                VariantGroupId.parse(idString) match {
                  case Left(_) => None
                  case Right(id) =>
                    HtsjdkUtils.liftOverKeepChromosome(liftOver, id.location) match {
                      case Left(_) => None
                      case Right(locationLiftedOver) =>
                        val idLiftedOver = id.copy(location = locationLiftedOver)
                        Some(row.withValue(idCol, idLiftedOver.toString))
                    }
                }
              case None => Some(row)
            }
          case None => Some(row)
        }
      rowNewOpt.flatMap { row =>
        chromPosColsOpt match {
          case Some(ChromPosCols(chromCol, posCol)) =>
            val valueMap = row.valueMap
            (valueMap.get(chromCol), valueMap.get(posCol)) match {
              case (Some(chromString), Some(posString)) =>
                Chromosome.parse(chromString) match {
                  case Left(_) => None
                  case Right(chromosome) =>
                    NumberParser.UnsignedIntParser.parseOpt(posString) match {
                      case Some(pos) =>
                        val locationOriginal = Location(chromosome, pos)
                        HtsjdkUtils.liftOverKeepChromosome(liftOver, locationOriginal) match {
                          case Left(_) => None
                          case Right(locationLiftedOver) =>
                            Some(row.withValue(posCol, locationLiftedOver.position.toString))
                        }
                      case None => None
                    }
                }
              case _ => Some(row)
            }
          case None => Some(row)
        }
      }
      rowNewOpt match {
        case Some(row) => Iterator.single(row)
        case None => Iterator.empty
      }
    }
    reader.flatMap(rowMapper).foreach(row => writer.addRow(row))
    Result(command, success = true)
  }

  case class Result(command: LiftoverTsvCommand, success: Boolean) extends ChowserExecuter.Result[LiftoverTsvCommand]

}
