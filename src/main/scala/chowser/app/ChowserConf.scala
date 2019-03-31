package chowser.app

import java.io.{File => JFile}

import better.files._
import chowser.cmd.{ChowserCommand, TsvFilterCommand}
import chowser.filter.DoubleFilters
import org.rogach.scallop.{ScallopConf, Subcommand}

class ChowserConf(args: Array[String]) extends ScallopConf(args) {
  version(ChowserAppInfo.fullName)
  banner("Usage: chowser tsv|vcf ... ")
  val tsv = new Subcommand("tsv") {
    banner("Usage: chowser tsv filter [OPTIONS] \nConsume tab-separated file")
    val filter = new Subcommand("filter") {
      banner("usage: chowser tsv filter [OPTIONS]\nFilter records of tab-separated file")
      val in = opt[JFile]("in", required = true, descr = "Input file")
      val out = opt[JFile]("out", required = true, descr = "Output file")
      val col = opt[String]("col", required = true, descr = "Name of column to apply condition")
      val lt = opt[Double]("lt", descr = "Retain records with value less than given value")
      val gt = opt[Double]("gt", descr = "Retain records with value greater than given value")
      requireAtLeastOne(lt, gt)
    }
    addSubcommand(filter)
  }
  addSubcommand(tsv)
  val variants = new Subcommand("genomics") {
    banner("Usage: chowser variants regions [OPTIONS]\nConsume file containing variants")
    val regions = new Subcommand("regions") {

    }
    addSubcommand(regions)
  }
  addSubcommand(variants)
  requireSubcommand()
  verify()

  def toChowserCommand: Either[String, ChowserCommand] = {
    subcommands match {
      case List(this.tsv, this.tsv.filter) =>
        val filter = tsv.filter
        val inFile = filter.in().toScala
        val outFile = filter.out().toScala
        val colName = filter.col()
        val upperLimitOpt = filter.lt.toOption.map(DoubleFilters.lessThan)
        val lowerLimitOpt = filter.gt.toOption.map(DoubleFilters.greaterThan)
        val numberFilter = (upperLimitOpt, lowerLimitOpt) match {
          case (Some(upperLimit), Some(lowerLimit)) => upperLimit && lowerLimit
          case (Some(upperLimit), None) => upperLimit
          case (None, Some(lowerLimit)) => lowerLimit
          case (None, None) => DoubleFilters.all
        }
        Right(TsvFilterCommand(inFile, outFile, colName, numberFilter))
      case _ => Left("Invalid combination of commands.")
    }
  }
}
