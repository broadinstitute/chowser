package chowser.util.io

import better.files.File

case class IoId(file: File)

object IoId {
  def apply(string: String): IoId = IoId(File(string))
}