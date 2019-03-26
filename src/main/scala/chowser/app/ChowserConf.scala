package chowser.app

import java.io.File

import org.rogach.scallop.{ScallopConf, Subcommand}

class ChowserConf(args: Array[String]) extends ScallopConf(args) {
  version(ChowserAppInfo.fullName)
  banner("Usage: chowser tsv|vcf ... ")
  val tsv = new Subcommand("tsv") {
    banner("Usage: chowser tsv filter [OPTIONS] \nConsume tab-separated file")
    val filter = new Subcommand("filter") {
      banner("usage: chowser tsv filter [OPTIONS]\nFilter records of tab-separated file")
      val in = opt[File]("in", descr = "Input file")
      val out = opt[File]("out", descr = "Output file")
      val col = opt[String]("col", descr = "Name of column to apply condition")
      val lt = opt[Double]("lt", descr = "Retain records with value less than given value")
      val gt = opt[Double]("gt", descr = "Retain records with value greater than given value")
    }
    addSubcommand(filter)
  }
  addSubcommand(tsv)
  verify()
}
