package chowser.util.io

import better.files.File

trait IoId {
  def file: File
}

trait InputId extends IoId

object InputId {
  def apply(string: String): InputId = FileInputId(File(string))
}

trait OutputId extends IoId {
  def appendLine(line: String): Unit
}

object OutputId {
  def apply(string: String): OutputId = FileOutputId(File(string))
}

trait FileIoId {
  def file: File
}

case class FileInputId(file: File) extends InputId with FileIoId

case class FileOutputId(file: File) extends OutputId with FileIoId {
  override def appendLine(line: String): Unit = file.appendLine(line)
}

