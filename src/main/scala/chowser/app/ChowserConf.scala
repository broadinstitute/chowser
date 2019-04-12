package chowser.app

import java.io.{File => JFile}

import better.files._
import chowser.cmd.{ChowserCommand, TsvFilterCommand, TsvSortCommand, VariantsForRegionCommand, VariantsRegionsCommand}
import chowser.filter.DoubleFilters
import org.rogach.scallop.{ScallopConf, Subcommand}

import scala.language.reflectiveCalls

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
    val sort = new Subcommand("sort") with OneInFile with OneOutFile {
      banner("usage: chowser tsv sort [OPTIONS]\nSort records of tab-separated file")
      val col = opt[String]("col", required = true, descr = "Name of column to sort by")
    }
    addSubcommand(sort)
  }
  addSubcommand(tsv)
  val variants = new Subcommand("variants") {
    banner("Usage: chowser variants regions [OPTIONS]\nConsume file containing variants")
    trait ChromPosCols {
      _: ScallopConf =>
      val chromCol = opt[String]("chrom-col", required = true, descr = "Name of column containing chromosome")
      val posCol = opt[String]("pos-col", required = true, descr = "Name of column containing position")
    }
    val regions = new Subcommand("regions") with OneInFile with OneOutFile with ChromPosCols {
      val radius = opt[Int]("radius", required = true, descr = "Minimum distance to be included on each side.")
    }
    addSubcommand(regions)
    val forRegion = new Subcommand("for-region")
      with OneInFile with OneOutFile with ChromPosCols {
      val chrom = opt[String]("chrom", required = true, descr = "Chromosome on which region lies.")
      val start = opt[Int](name = "start", required = true, descr = "Start position of region.")
      val end = opt[Int](name = "end", required = true, descr = "End position of region.")
    }
    addSubcommand(forRegion)
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
      case List(this.tsv, this.tsv.sort) =>
        val subcommand = tsv.sort
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        val colName = subcommand.col()
        Right(TsvSortCommand(inFile, outFile, colName))
      case List(this.variants, this.variants.regions) =>
        val subcommand = variants.regions
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        val chromColName = subcommand.chromCol()
        val posColName = subcommand.posCol()
        val radius  = subcommand.radius()
        Right(VariantsRegionsCommand(inFile, outFile, chromColName, posColName, radius))
      case List(this.variants, this.variants.forRegion) =>
        val subcommand = variants.forRegion
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        val chromColName = subcommand.chromCol()
        val posColName = subcommand.posCol()
        val chromosome = subcommand.chrom()
        val start = subcommand.start()
        val end = subcommand.end()
        Right(VariantsForRegionCommand(inFile, outFile, chromColName, posColName, chromosome, start, end))
      case _ => Left("Invalid combination of commands.")
    }
  }
}
