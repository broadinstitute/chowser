package chowser.util.io

import better.files.File

trait IoId {
  def file: File
}

trait InputId extends IoId

object InputId {
  def apply(string: String): InputId = FileInputId(File(string))
}

trait OutputId extends IoId

object OutputId {
  def apply(string: String): OutputId = FileOutputId(File(string))
}

case class FileInputId(file: File) extends InputId

case class FileOutputId(file: File) extends OutputId

