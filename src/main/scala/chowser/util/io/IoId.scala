package chowser.util.io

import java.io.{InputStream, PrintWriter}

import better.files.File

trait IoId {
  def asString: String

  override def toString: String = asString
}

trait InputId extends IoId {
  def newLineIterator(resourceConfig: ResourceConfig): Iterator[String]

  def newInputStream(resourceConfig: ResourceConfig): InputStream
}

object InputId {
  def apply(string: String): InputId = FileInputId(File(string))
}

trait OutputId extends IoId {
  def newPrintWriter(resourceConfig: ResourceConfig): PrintWriter
}

object OutputId {
  def apply(string: String): OutputId = FileOutputId(File(string))
}

trait FileIoId {
  def file: File

  def fileDeprecated: File = file
}

case class FileInputId(file: File) extends InputId with FileIoId {
  override def asString: String = file.toString()

  override def newLineIterator(resourceConfig: ResourceConfig): Iterator[String] = fileDeprecated.lineIterator

  override def newInputStream(resourceConfig: ResourceConfig): InputStream = file.inputStream.get
}

case class FileOutputId(file: File) extends OutputId with FileIoId {
  override def asString: String = file.toString()

  override def newPrintWriter(resourceConfig: ResourceConfig): PrintWriter = fileDeprecated.newPrintWriter()
}

