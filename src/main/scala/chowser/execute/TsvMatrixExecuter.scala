package chowser.execute

import better.files.File
import chowser.cmd.TsvMatrixCommand
import chowser.tsv.{TsvReader, TsvWriter}

object TsvMatrixExecuter extends ChowserExecuter[TsvMatrixCommand] {

  def execute(command: TsvMatrixCommand): Result = {
    import command._
    val idList = TsvReader.forSimpleHeaderLine(idsFile).flatMap(_.valueMap.get(idCol)).toSeq
    val matrix = new Matrix(idList, "0.0")
    val valueReader = TsvReader.forSimpleHeaderLine(valuesFile)
    valueReader.foreach { row =>
      matrix.put(row.string(idCol1), row.string(idCol2), row.string(valueCol))
    }
    matrix.writeTo(outFile)
    Result(command, success = true)
  }

  class Matrix(ids: Seq[String], default: String) {
    val size: Int = ids.size
    val elements: Array[Array[String]] = Array.fill(size)(Array.fill(size)(default))
    val idToIndex: Map[String, Int] = ids.zipWithIndex.toMap

    def put(id1: String, id2: String, value: String): Unit = {
      elements(id1)(id2) = value
      elements(id2)(id1) = value
    }

    def writeTo(file: File): Unit = {
      val writer = TsvWriter(file, Seq.empty)
      elements.foreach(writer.addRow(_))
    }
  }

  case class Result(command: TsvMatrixCommand, success: Boolean) extends ChowserExecuter.Result[TsvMatrixCommand]

}
