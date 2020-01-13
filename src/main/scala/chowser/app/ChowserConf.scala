package chowser.app

import chowser.cmd.LiftoverTsvCommand.ChromPosCols
import chowser.cmd._
import chowser.filter.{DoubleFilters, Filter}
import chowser.genomics.{Chromosome, Region}
import chowser.util.io.{InputId, OutputId, ResourceConfig}
import org.rogach.scallop.{ScallopConf, Subcommand, ValueConverter, singleArgConverter}

import scala.language.reflectiveCalls

class ChowserConf(args: Array[String]) extends ScallopConf(args) {
  version(ChowserAppInfo.fullName)
  banner("Usage: chowser tsv|vcf ... ")

  implicit val inputIdConverter: ValueConverter[InputId] = singleArgConverter(InputId.apply)
  implicit val outputIdConverter: ValueConverter[OutputId] = singleArgConverter(OutputId.apply)

  trait OneInFile {
    _: ScallopConf =>
    val in = opt[InputId]("in", required = true, descr = "Input file")
  }

  trait OneOutFile {
    _: ScallopConf =>
    val out = opt[OutputId]("out", required = true, descr = "Output file")
  }

  trait KeyFile {
    _: ScallopConf =>
    val keyFile = opt[InputId]("key-file", required = false, descr = "Key file for authentication.")
  }

  val tsv = new Subcommand("tsv") {
    banner("Usage: chowser tsv filter [OPTIONS] \nConsume tab-separated file")
    val range = new Subcommand("range") with OneInFile with OneOutFile with KeyFile {
      banner("usage: chowser tsv range [OPTIONS]\nFilter records by numeric range")
      val col = opt[String]("col", required = true, descr = "Name of column to apply condition")
      val lt = opt[Double]("lt", descr = "Retain records with value less than given value")
      val gt = opt[Double]("gt", descr = "Retain records with value greater than given value")
      requireAtLeastOne(lt, gt)
    }
    addSubcommand(range)
    val slice = new Subcommand("slice") with OneInFile with OneOutFile with KeyFile {
      banner("usage: chowser tsv slice [OPTIONS]\nFilter records by numeric range")
      val col = opt[String]("col", required = true, descr = "Name of column to apply condition")
      val value = opt[String]("value", required = true, descr = "Required value")
    }
    addSubcommand(slice)
    val sort = new Subcommand("sort") with OneInFile with OneOutFile with KeyFile {
      banner("usage: chowser tsv sort [OPTIONS]\nSort records of tab-separated file")
      val col = opt[String]("col", required = true, descr = "Name of column to sort by")
    }
    addSubcommand(sort)
    val sortIds = new Subcommand("sort-ids") with OneInFile with OneOutFile with KeyFile {
      banner("usage: chowser tsv sort-ids [OPTIONS]\nSort records of tab-separated file")
      val col = opt[String]("col", required = true, descr = "Name of column of variant ids to sort by")
    }
    addSubcommand(sortIds)
    val extractUnique = new Subcommand("extract-unique")
      with OneInFile with OneOutFile with KeyFile {
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

    val regions = new Subcommand("regions")
      with OneInFile with OneOutFile with ChromPosCols  with KeyFile  {
      val radius = opt[Int]("radius", required = true, descr = "Minimum distance to be included on each side.")
    }
    addSubcommand(regions)
    val forRegion = new Subcommand("for-region")
      with OneInFile with OneOutFile with ChromPosCols with KeyFile {
      val chrom = opt[String](name = "chrom", required = true, descr = "Chromosome on which region lies.")
      val start = opt[Int](name = "start", required = true, descr = "Start position of region.")
      val end = opt[Int](name = "end", required = true, descr = "End position of region.")
    }
    addSubcommand(forRegion)
    val forRegionById = new Subcommand("for-region-by-id")
      with OneInFile with OneOutFile with IdCol with KeyFile {
      val chrom = opt[String](name = "chrom", required = true, descr = "Chromosome on which region lies.")
      val start = opt[Int](name = "start", required = true, descr = "Start position of region.")
      val end = opt[Int](name = "end", required = true, descr = "End position of region.")
    }
    addSubcommand(forRegionById)
    val canonicalizeVcf = new Subcommand("canonicalize-vcf")
      with OneInFile with OneOutFile with KeyFile {
    }
    addSubcommand(canonicalizeVcf)
    val canonicalizeTsv = new Subcommand("canonicalize-tsv")
      with OneInFile with OneOutFile with IdCol with ChromPosCols with KeyFile {
      val refCol =
        opt[String](name = "ref-col", required = true, descr = "Name of column containing reference allele.")
      val altCol =
        opt[String](name = "alt-col", required = true, descr = "Name of column containing alternate allele.")
    }
    addSubcommand(canonicalizeTsv)
    val matchVcfTsv = new Subcommand("match-vcf-tsv") with KeyFile {
      val vcf = opt[InputId](name = "vcf", required = true, descr = "Input VCF file")
      val tsv = opt[InputId](name = "tsv", required = true, descr = "Input TSV file")
      val idCol = opt[String](name = "id-col", required = true, descr = "Variant id column name in TSV file")
      val inBoth = opt[OutputId](name = "in-both", descr = "Output file with variants both in VCF and TSV file.")
      val vcfOnly = opt[OutputId](name = "vcf-only", descr = "Output file with variants only in VCF file.")
      val tsvOnly = opt[OutputId](name = "tsv-only", descr = "Output file with variants only in TSV file.")
      requireAtLeastOne(inBoth, vcfOnly, tsvOnly)
    }
    addSubcommand(matchVcfTsv)
    val matchTsvTsv = new Subcommand("match-tsv-tsv") with KeyFile {
      val tsv1 = opt[InputId](name = "tsv1", required = true, descr = "Input VCF file")
      val tsv2 = opt[InputId](name = "tsv2", required = true, descr = "Input TSV file")
      val idCol1 = opt[String](name = "id-col1", required = true, descr = "Variant id column name in TSV file 1")
      val idCol2 = opt[String](name = "id-col2", required = true, descr = "Variant id column name in TSV file 2")
      val inBoth = opt[OutputId](name = "in-both", descr = "Output file with variants in both TSV files.")
      val inOneOnly = opt[OutputId](name = "tsv1-only", descr = "Output file with variants only in TSV file 1.")
      val inTwoOnly = opt[OutputId](name = "tsv2-only", descr = "Output file with variants only in TSV file 2.")
      requireAtLeastOne(inBoth, inOneOnly, inTwoOnly)
    }
    addSubcommand(matchTsvTsv)
    val selectTsv = new Subcommand("select-tsv") with OneOutFile with KeyFile {
      val data = opt[InputId](name = "data", required = true, descr = "File with data to to select.")
      val selection = opt[InputId](name = "selection", required = true, descr = "Ids of variants to select..")
      val idColData = opt[String](name = "id-col-data", required = true, descr = "Column with variant ids in data.")
      val idColSelection =
        opt[String](name = "id-col-selection", required = true, descr = "Column with variant ids in selection.")
    }
    addSubcommand(selectTsv)
    val selectVcf = new Subcommand("select-vcf") with OneOutFile with KeyFile {
      val data = opt[InputId](name = "data", required = true, descr = "File with data to to select.")
      val selection = opt[InputId](name = "selection", required = true, descr = "Ids of variants to select..")
      val idColSelection =
        opt[String](name = "id-col-selection", required = true, descr = "Column with variant ids in selection.")
    }
    addSubcommand(selectVcf)
  }
  addSubcommand(variants)
  val caviar = new Subcommand("caviar") {
    val matrix = new Subcommand("matrix") with OneOutFile with KeyFile {
      banner("usage: chowser tsv matrix [OPTIONS]\nReshape list of values into matrix form")
      val valuesFile = opt[InputId]("values-file", required = true, descr = "File with values")
      val idsFile = opt[InputId]("ids-file", required = true, descr = "VCF File for ids")
      val valueCol = opt[String]("value-col", required = true, descr = "Name of value column in values file.")
      val idCol1 = opt[String]("id-col1", required = true, descr = "Name of first id column in values file.")
      val idCol2 = opt[String]("id-col2", required = true, descr = "Name of second id column in values file.")
    }
    addSubcommand(matrix)
    val pToZ = new Subcommand("p-to-z") with OneInFile with OneOutFile with KeyFile {
      val idCol = opt[String]("id-col", required = true, descr = "Name of id column in p-values file.")
      val pCol = opt[String]("p-col", required = true, descr = "Name of p-value column in p-values file.")
    }
    addSubcommand(pToZ)
  }
  addSubcommand(caviar)
  val liftover = new Subcommand("liftover") {
    val tsv = new Subcommand("tsv") with OneInFile with OneOutFile with KeyFile {
      val chainFile = opt[InputId]("chain-file", required = true, descr = "Chain file")
      val idCol = opt[String]("id-col", descr = "Name of id column")
      val chromCol = opt[String]("chrom-col", descr = "Name of chromosome column")
      val posCol = opt[String]("pos-col", descr = "Name of pos column")
      requireAtLeastOne(idCol, posCol)
      codependent(chromCol, posCol)
    }
    addSubcommand(tsv)
  }
  addSubcommand(liftover)
  val shell = new Subcommand("shell") with KeyFile
  addSubcommand(shell)
  requireSubcommand()
  verify()

  def toChowserCommand: Either[String, ChowserCommand] = {
    subcommands match {
      case List(this.tsv, this.tsv.range) =>
        val subcommand = tsv.range
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val colName = subcommand.col()
        val upperLimitOpt = subcommand.lt.toOption.map(DoubleFilters.lessThan)
        val lowerLimitOpt = subcommand.gt.toOption.map(DoubleFilters.greaterThan)
        val numberFilter = (upperLimitOpt, lowerLimitOpt) match {
          case (Some(upperLimit), Some(lowerLimit)) => upperLimit && lowerLimit
          case (Some(upperLimit), None) => upperLimit
          case (None, Some(lowerLimit)) => lowerLimit
          case (None, None) => DoubleFilters.all
        }
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(TsvRangeCommand(resourceConfig, inFile, outFile, colName, numberFilter))
      case List(this.tsv, this.tsv.slice) =>
        val subcommand = tsv.slice
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val colName = subcommand.col()
        val value = subcommand.value()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(TsvSliceCommand(resourceConfig, inFile, outFile, colName, Filter.Equal(value)))
      case List(this.tsv, this.tsv.sort) =>
        val subcommand = tsv.sort
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val colName = subcommand.col()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(TsvSortCommand(resourceConfig, inFile, outFile, colName))
      case List(this.tsv, this.tsv.sortIds) =>
        val subcommand = tsv.sortIds
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val colName = subcommand.col()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(TsvSortIdsCommand(resourceConfig, inFile, outFile, colName))
      case List(this.tsv, this.tsv.extractUnique) =>
        val subcommand = tsv.extractUnique
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val keyFileOpt = subcommand.keyFile.toOption
        val colName = subcommand.col()
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(TsvExtractUniqueCommand(resourceConfig, inFile, outFile, colName))
      case List(this.variants, this.variants.regions) =>
        val subcommand = variants.regions
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val chromColName = subcommand.chromCol()
        val posColName = subcommand.posCol()
        val radius = subcommand.radius()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(VariantsRegionsCommand(resourceConfig, inFile, outFile, chromColName, posColName, radius))
      case List(this.variants, this.variants.forRegion) =>
        val subcommand = variants.forRegion
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val chromColName = subcommand.chromCol()
        val posColName = subcommand.posCol()
        val chromosomeEither = Chromosome.parse(subcommand.chrom())
        val start = subcommand.start()
        val end = subcommand.end()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        chromosomeEither match {
          case Left(message) => Left("Couldn't parse chromosome:" + message)
          case Right(chromosome) =>
            Right(
              VariantsForRegionCommand(
                resourceConfig, inFile, outFile, chromColName, posColName, Region(chromosome, start, end)
              )
            )
        }
      case List(this.variants, this.variants.forRegionById) =>
        val subcommand = variants.forRegionById
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val idColName = subcommand.idCol()
        val chromosomeEither = Chromosome.parse(subcommand.chrom())
        val start = subcommand.start()
        val end = subcommand.end()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        chromosomeEither match {
          case Left(message) => Left("Couldn't parse chromosome:" + message)
          case Right(chromosome) =>
            Right(VariantsForRegionByIdCommand(resourceConfig, inFile, outFile, idColName, Region(chromosome, start, end)))
        }
      case List(this.variants, this.variants.canonicalizeVcf) =>
        val subcommand = variants.canonicalizeVcf
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(VariantsCanonicalizeVcfCommand(resourceConfig, inFile, outFile))
      case List(this.variants, this.variants.canonicalizeTsv) =>
        val subcommand = variants.canonicalizeTsv
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val idCol = subcommand.idCol()
        val chromCol = subcommand.chromCol()
        val posCol = subcommand.posCol()
        val refCol = subcommand.refCol()
        val altCol = subcommand.altCol()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(VariantsCanonicalizeTsvCommand(resourceConfig, inFile, outFile, idCol, chromCol, posCol, refCol, altCol))
      case List(this.variants, this.variants.matchVcfTsv) =>
        val subcommand = variants.matchVcfTsv
        val vcfFile = subcommand.vcf()
        val tsvFile = subcommand.tsv()
        val idColName = subcommand.idCol()
        val inBothOpt = subcommand.inBoth.toOption
        val vcfOnly = subcommand.vcfOnly.toOption
        val tsvOnly = subcommand.tsvOnly.toOption
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(
          VariantsMatchVcfTsvCommand(resourceConfig, vcfFile, tsvFile, idColName, inBothOpt, vcfOnly, tsvOnly)
        )
      case List(this.variants, this.variants.matchTsvTsv) =>
        val subcommand = variants.matchTsvTsv
        val vcfFile = subcommand.tsv1()
        val tsvFile = subcommand.tsv2()
        val idCol1 = subcommand.idCol1()
        val idCol2 = subcommand.idCol2()
        val inBothOpt = subcommand.inBoth.toOption
        val inOneOnly = subcommand.inOneOnly.toOption
        val inTwoOnly = subcommand.inTwoOnly.toOption
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(
          VariantsMatchTsvTsvCommand(resourceConfig, vcfFile, tsvFile, idCol1, idCol2, inBothOpt, inOneOnly, inTwoOnly)
        )
      case List(this.variants, this.variants.selectTsv) =>
        val subcommand = variants.selectTsv
        val dataFile = subcommand.data()
        val selectionFile = subcommand.selection()
        val outFile = subcommand.out()
        val idColData = subcommand.idColData()
        val idColSelection = subcommand.idColSelection()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(VariantsSelectTsvCommand(resourceConfig, dataFile, selectionFile, outFile, idColData, idColSelection))
      case List(this.variants, this.variants.selectVcf) =>
        val subcommand = variants.selectVcf
        val dataFile = subcommand.data()
        val selectionFile = subcommand.selection()
        val outFile = subcommand.out()
        val idColSelection = subcommand.idColSelection()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(VariantsSelectVcfCommand(resourceConfig, dataFile, selectionFile, outFile, idColSelection))
      case List(this.caviar, this.caviar.matrix) =>
        val subcommand = caviar.matrix
        val valuesFile = subcommand.valuesFile()
        val idsFile = subcommand.idsFile()
        val outFile = subcommand.out()
        val idCol1 = subcommand.idCol1()
        val idCol2 = subcommand.idCol2()
        val valueCol = subcommand.valueCol()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(TsvMatrixCommand(resourceConfig, valuesFile, idsFile, outFile, idCol1, idCol2, valueCol))
      case List(this.caviar, this.caviar.pToZ) =>
        val subcommand = caviar.pToZ
        val inFile = subcommand.in()
        val outFile = subcommand.out()
        val idCol = subcommand.idCol()
        val pCol = subcommand.pCol()
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(CaviarPToZCommand(resourceConfig, inFile, outFile, idCol, pCol))
      case List(this.liftover, this.liftover.tsv) =>
        val subcommand = liftover.tsv
        val inFile = subcommand.in()
        val chainFile = subcommand.chainFile()
        val outFile = subcommand.out()
        val idColOpt = subcommand.idCol.toOption
        val chromColOpt = subcommand.chromCol.toOption
        val posColOpt = subcommand.posCol.toOption
        val chromPosColsOpt = for(chromCol <- chromColOpt; posCol <- posColOpt) yield ChromPosCols(chromCol, posCol)
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(LiftoverTsvCommand(resourceConfig, inFile, chainFile, outFile, idColOpt, chromPosColsOpt))
      case List(this.shell) =>
        val subcommand = shell
        val keyFileOpt = subcommand.keyFile.toOption
        val resourceConfig = ResourceConfig.forKeyFileOpt(keyFileOpt)
        Right(ShellCommand(resourceConfig))
      case _ => Left("Invalid combination of commands.")
    }
  }
}
