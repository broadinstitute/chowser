package chowser.util.io

import java.io.PrintWriter

import better.files.File

trait IoId {
  def file: File
}

trait InputId extends IoId {
  def newLineIterator(resourceConfig: ResourceConfig): Iterator[String]
}

object InputId {
  def apply(string: String): InputId = FileInputId(File(string))
}

trait OutputId extends IoId {
  def newPrintWriter(resourceConfig: ResourceConfig): PrintWriter
  def appendLine(line: String): Unit
}

object OutputId {
  def apply(string: String): OutputId = FileOutputId(File(string))
}

trait FileIoId {
  def file: File
}

case class FileInputId(file: File) extends InputId with FileIoId {
  override def newLineIterator(resourceConfig: ResourceConfig): Iterator[String] = file.lineIterator
}

case class FileOutputId(file: File) extends OutputId with FileIoId {
  override def appendLine(line: String): Unit = file.appendLine(line)

  override def newPrintWriter(resourceConfig: ResourceConfig): PrintWriter = file.newPrintWriter()
}

