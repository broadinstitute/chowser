package chowser.app

import java.io.{File => JFile}

import better.files._
import chowser.cmd.{ChowserCommand, CompareVariantsCommand, TsvFilterCommand, TsvSortCommand, VariantsForRegionCommand, VariantsRegionsCommand}
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
      val chrom = opt[String](name = "chrom", required = true, descr = "Chromosome on which region lies.")
      val start = opt[Int](name = "start", required = true, descr = "Start position of region.")
      val end = opt[Int](name = "end", required = true, descr = "End position of region.")
    }
    addSubcommand(forRegion)
  }
  addSubcommand(variants)
  val compare = new Subcommand("compare") {
    banner("Usage: chowser compare variants [OPTIONS]\nCompare a VCF and a TSV file, both sorted, containing variants")
    val variants = new Subcommand("variants") {
      val vcf = opt[JFile](name = "vcf", required = true, descr = "Input VCF file")
      val tsv = opt[JFile](name = "tsv", required = true, descr = "Input TSV file")
      val idCol = opt[String](name = "id-col", required = true, descr = "Variant id column name in TSV file")
      val chromCol = opt[String](name = "chrom-col", required = true, descr = "Chromosome column name in TSV file")
      val posCol = opt[String](name = "pos-col", required = true, descr = "Position column name in TSV file")
      val inBoth = opt[JFile](name = "in-both", descr = "Output file with variants both in VCF and TSV file.")
      val vcfOnly = opt[JFile](name = "vcf-only", descr = "Output file with variants only in VCF file.")
      val tsvOnly = opt[JFile](name = "tsv-only", descr = "Output file with variants only in TSV file.")
      requireAtLeastOne(inBoth, vcfOnly, tsvOnly)
    }
    addSubcommand(variants)
  }
  addSubcommand(compare)
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
      case List(this.compare, this.compare.variants) =>
        val subcommand = compare.variants
        val vcfFile = subcommand.vcf().toScala
        val tsvFile = subcommand.tsv().toScala
        val idColName = subcommand.idCol()
        val chromColName = subcommand.chromCol()
        val posColName = subcommand.posCol()
        val inBothOpt = subcommand.inBoth.toOption.map(_.toScala)
        val vcfOnly = subcommand.vcfOnly.toOption.map(_.toScala)
        val tsvOnly = subcommand.tsvOnly.toOption.map(_.toScala)
        Right(
          CompareVariantsCommand(vcfFile, tsvFile, idColName, chromColName, posColName, inBothOpt, vcfOnly, tsvOnly)
        )
      case _ => Left("Invalid combination of commands.")
    }
  }
}
