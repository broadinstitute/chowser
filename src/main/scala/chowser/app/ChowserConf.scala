package chowser.app

import java.io.{File => JFile}

import better.files._
import chowser.cmd.{ChowserCommand, MatchVariantsCommand, TsvExtractUniqueCommand, TsvFilterCommand, TsvSortCommand, VariantsCanonicalizeTsvCommand, VariantsCanonicalizeVcfCommand, VariantsForRegionByIdCommand, VariantsForRegionCommand, VariantsRegionsCommand}
import chowser.filter.DoubleFilters
import chowser.genomics.{Chromosome, Region}
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
    val extractUnique = new Subcommand("extract-unique") with OneInFile with OneOutFile {
      banner("usage: chowser tsv extract-unique [OPTIONS]\nExtract unique values of a column of tab-separated file")
      val col = opt[String]("col", required = true, descr = "Name of column")
    }
    addSubcommand(extractUnique)
  }
  addSubcommand(tsv)
  val variants = new Subcommand("variants") {
    banner("Usage: chowser variants regions [OPTIONS]\nConsume file containing variants")
    trait ChromPosCols {
      _: ScallopConf =>
      val chromCol = opt[String]("chrom-col", required = true, descr = "Name of column containing chromosome")
      val posCol = opt[String]("pos-col", required = true, descr = "Name of column containing position")
    }
    trait IdCol {
      _: ScallopConf =>
      val idCol = opt[String]("id-col", required = true, descr = "Name of column containing variant ids")
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
    val forRegionById = new Subcommand("for-region-by-id")
      with OneInFile with OneOutFile with IdCol {
      val chrom = opt[String](name = "chrom", required = true, descr = "Chromosome on which region lies.")
      val start = opt[Int](name = "start", required = true, descr = "Start position of region.")
      val end = opt[Int](name = "end", required = true, descr = "End position of region.")
    }
    addSubcommand(forRegionById)
    val canonicalizeTsv = new Subcommand("canonicalize-tsv")
      with OneInFile with OneOutFile with IdCol with ChromPosCols {
      val refCol =
        opt[String](name = "ref-col", required = true, descr = "Name of column containing reference allele.")
      val altCol =
        opt[String](name = "alt-col", required = true, descr = "Name of column containing alternate allele.")
    }
    addSubcommand(canonicalizeTsv)
    val canonicalizeVcf = new Subcommand("canonicalize-vcf")
      with OneInFile with OneOutFile {
    }
    addSubcommand(canonicalizeVcf)
  }
  addSubcommand(variants)
  val compare = new Subcommand("match") {
    banner("Usage: chowser compare variants [OPTIONS]\nCompare a VCF and a TSV file, both sorted, containing variants")
    val variants = new Subcommand("variants") {
      val vcf = opt[JFile](name = "vcf", required = true, descr = "Input VCF file")
      val tsv = opt[JFile](name = "tsv", required = true, descr = "Input TSV file")
      val idCol = opt[String](name = "id-col", required = true, descr = "Variant id column name in TSV file")
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
      case List(this.tsv, this.tsv.extractUnique) =>
        val subcommand = tsv.extractUnique
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        val colName = subcommand.col()
        Right(TsvExtractUniqueCommand(inFile, outFile, colName))
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
        val chromosomeEither = Chromosome.parse(subcommand.chrom())
        val start = subcommand.start()
        val end = subcommand.end()
        chromosomeEither match {
          case Left(message) => Left("Couldn't parse chromosome:" + message)
          case Right(chromosome) =>
            Right(VariantsForRegionCommand(inFile, outFile, chromColName, posColName, Region(chromosome, start, end)))
        }
      case List(this.variants, this.variants.forRegionById) =>
        val subcommand = variants.forRegionById
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        val idColName = subcommand.idCol()
        val chromosomeEither = Chromosome.parse(subcommand.chrom())
        val start = subcommand.start()
        val end = subcommand.end()
        chromosomeEither match {
          case Left(message) => Left("Couldn't parse chromosome:" + message)
          case Right(chromosome) =>
            Right(VariantsForRegionByIdCommand(inFile, outFile, idColName, Region(chromosome, start, end)))
        }
      case List(this.variants, this.variants.canonicalizeTsv) =>
        val subcommand = variants.canonicalizeTsv
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        val idCol = subcommand.idCol()
        val chromCol = subcommand.chromCol()
        val posCol = subcommand.posCol()
        val refCol = subcommand.refCol()
        val altCol = subcommand.altCol()
        Right(VariantsCanonicalizeTsvCommand(inFile, outFile, idCol, chromCol, posCol, refCol, altCol))
      case List(this.variants, this.variants.canonicalizeVcf) =>
        val subcommand = variants.canonicalizeVcf
        val inFile = subcommand.in().toScala
        val outFile = subcommand.out().toScala
        Right(VariantsCanonicalizeVcfCommand(inFile, outFile))
      case List(this.compare, this.compare.variants) =>
        val subcommand = compare.variants
        val vcfFile = subcommand.vcf().toScala
        val tsvFile = subcommand.tsv().toScala
        val idColName = subcommand.idCol()
        val inBothOpt = subcommand.inBoth.toOption.map(_.toScala)
        val vcfOnly = subcommand.vcfOnly.toOption.map(_.toScala)
        val tsvOnly = subcommand.tsvOnly.toOption.map(_.toScala)
        Right(
          MatchVariantsCommand(vcfFile, tsvFile, idColName, inBothOpt, vcfOnly, tsvOnly)
        )
      case _ => Left("Invalid combination of commands.")
    }
  }
}
