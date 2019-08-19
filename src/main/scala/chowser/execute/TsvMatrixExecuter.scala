package chowser.execute

import better.files.File
import chowser.cmd.TsvMatrixCommand
import chowser.tsv.BasicTsvReader.LineParser
import chowser.tsv.{BasicTsvReader, TsvWriter}
import htsjdk.variant.vcf.VCFFileReader

import scala.collection.JavaConverters.asScalaIteratorConverter

object TsvMatrixExecuter extends ChowserExecuter[TsvMatrixCommand] {

  def execute(command: TsvMatrixCommand): Result = {
    import command._
    val vcfReader = new VCFFileReader(idsFile.path, false)
    val idList = vcfReader.iterator().asScala.map(_.getID).toSeq
    val matrix = new Matrix(idList, "0.0", "1.0")
    val valueReader = BasicTsvReader.forSimpleHeaderLine(valuesFile, LineParser.whitespace)
    valueReader.foreach { row =>
      matrix.put(row.string(idCol1), row.string(idCol2), row.string(valueCol))
    }
    matrix.writeTo(outFile)
    Result(command, success = true)
  }

  class Matrix(ids: Seq[String], default: String, diagonal: String) {
    val size: Int = ids.size
    val elements: Array[Array[String]] = {
      val elementsTmp = Array.fill(size)(Array.fill(size)(default))
      for(i <- 0 until size) {
        elementsTmp(i)(i) = diagonal
      }
      elementsTmp
    }
    val idToIndex: Map[String, Int] = ids.zipWithIndex.toMap

    def put(id1: String, id2: String, value: String): Unit = {
      (idToIndex.get(id1), idToIndex.get(id2)) match {
        case (Some(index1), Some(index2)) =>
          elements(index1)(index2) = value
          elements(index2)(index1) = value
        case _ => ()

      }
    }

    def writeTo(file: File): Unit = {
      val writer = TsvWriter(file, Seq.empty)
      elements.foreach(writer.addRow(_))
    }
  }

  case class Result(command: TsvMatrixCommand, success: Boolean) extends ChowserExecuter.Result[TsvMatrixCommand]

}
