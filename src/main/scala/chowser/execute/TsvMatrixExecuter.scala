package chowser.execute

import better.files.File
import chowser.cmd.TsvMatrixCommand
import chowser.execute.ChowserExecuter.Result
import chowser.tsv.BasicTsvReader.LineParser
import chowser.tsv.{BasicTsvReader, TsvHeader, TsvRow, TsvWriter}
import chowser.util.io.{FileInputId, OutputId}
import htsjdk.variant.vcf.VCFFileReader
import org.broadinstitute.yootilz.core.snag.Snag

import scala.jdk.CollectionConverters._

object TsvMatrixExecuter extends ChowserExecuter[TsvMatrixCommand] {

  override def execute(command: TsvMatrixCommand): Either[Snag, Result] = {
    import command._
    idsFile match {
      case FileInputId(idsFileFile) =>
        val vcfReader = new VCFFileReader(idsFileFile.path, false)
        val idList = vcfReader.iterator().asScala.map(_.getID).toSeq
        val matrix = new Matrix(idList, "0.0", "1.0")
        val valueReader = BasicTsvReader.forSimpleHeaderLine(valuesFile, LineParser.whitespace)
        valueReader.foreach { row =>
          matrix.put(row.string(idCol1), row.string(idCol2), row.string(valueCol))
        }
        matrix.writeTo(outFile)
        Right(Result.Done)
      case _ => Left(Snag(s"This feature needs a local ids file, but $idsFile is not."))
    }
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

    def writeTo(file: OutputId): Unit = {
      val writer = TsvWriter(file, TsvHeader.empty)
      elements.foreach(values => writer.addRow(TsvRow(values)))
    }
  }

}
