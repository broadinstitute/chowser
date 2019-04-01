package chowser.app

import java.io.{File => JFile}

import better.files._
import chowser.cmd.{ChowserCommand, TsvFilterCommand, VariantsRegionsCommand}
import chowser.filter.DoubleFilters
import org.rogach.scallop.{ScallopConf, Subcommand}

class ChowserConf(args: Array[String]) extends ScallopConf(args) {
  version(ChowserAppInfo.fullName)
  banner("Usage: chowser tsv|vcf ... ")

  trait OneInFile {
    _: ScallopConf =>
    val in = opt[JFile]("in", required = true, descr = "Input file")
  }

  trait OneOutFile {
    _: ScallopConf =>
    val out = opt[JFile]("out", required = true, descr = "Output file")
  }

  val tsv = new Subcommand("tsv") {
    banner("Usage: chowser tsv filter [OPTIONS] \nConsume tab-separated file")
    val filter = new Subcommand("filter") with OneInFile with OneOutFile {
      banner("usage: chowser tsv filter [OPTIONS]\nFilter records of tab-separated file")
      val col = opt[String]("col", required = true, descr = "Name of column to apply condition")
      val lt = opt[Double]("lt", descr = "Retain records with value less than given value")
      val gt = opt[Double]("gt", descr = "Retain records with value greater than given value")
      requireAtLeastOne(lt, gt)
    }
    addSubcommand(filter)
  }
  addSubcommand(tsv)
  val variants = new Subcommand("variants") {
    banner("Usage: chowser variants regions [OPTIONS]\nConsume file containing variants")
    val regions = new Subcommand("regions") with OneInFile with OneOutFile {
      val chrom = opt[String]("chrom", required = true, descr = "Name of column containing chromosome")
      val pos = opt[String]("pos", required = true, descr = "Name of column containing position")
      val radius = opt[Int]("radius", required = true, descr = "Minimum distance to be included on each side.")
    }
    addSubcommand(regions)
  }
  addSubcommand(variants)
  requireSubcommand()
  verify()

  def toChowserCommand: Either[String, ChowserCommand] = {
    subcommands match {
      case List(this.tsv, this.tsv.filter) =>
        val subcommand = tsv.filter
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        val colName = subcommand.col()
        val upperLimitOpt = subcommand.lt.toOption.map(DoubleFilters.lessThan)
        val lowerLimitOpt = subcommand.gt.toOption.map(DoubleFilters.greaterThan)
        val numberFilter = (upperLimitOpt, lowerLimitOpt) match {
          case (Some(upperLimit), Some(lowerLimit)) => upperLimit && lowerLimit
          case (Some(upperLimit), None) => upperLimit
          case (None, Some(lowerLimit)) => lowerLimit
          case (None, None) => DoubleFilters.all
        }
        Right(TsvFilterCommand(inFile, outFile, colName, numberFilter))
      case List(this.variants, this.variants.regions) =>
        val subcommand = variants.regions
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        val chromColName = subcommand.chrom()
        val posColName = subcommand.pos()
        val radius  = subcommand.radius()
        Right(VariantsRegionsCommand(inFile, outFile, chromColName, posColName, radius))
      case _ => Left("Invalid combination of commands.")
    }
  }
}
