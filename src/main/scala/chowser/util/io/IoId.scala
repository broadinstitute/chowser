package chowser.util.io

import java.io.{BufferedReader, InputStream, PrintWriter}
import java.nio.channels.Channels
import java.nio.charset.Charset
import scala.jdk.CollectionConverters._

import better.files.File
import com.google.cloud.storage.{Blob, BlobId}

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

trait GcpBlobId {
  def blobId: BlobId
  def getBlob(resourceConfig: ResourceConfig): Blob = {
    (new Blob.Builder()).setBlobId(blobId).build() // TODO: credendials
  }
}

case class GcpBlobInputId(blobId: BlobId) extends GcpBlobId with InputId {
  override def newLineIterator(resourceConfig: ResourceConfig): Iterator[String] = {
    val reader = new BufferedReader(Channels.newReader((new Blob.Builder).setBlobId(blobId).build().reader(), "UTF-8"))
    reader.lines().iterator().asScala
  }

  override def newInputStream(resourceConfig: ResourceConfig): InputStream = ???

  override def asString: String = ???
}

case class GcpBlobOutputId(blobId: BlobId) extends GcpBlobId with OutputId {
  override def newPrintWriter(resourceConfig: ResourceConfig): PrintWriter = ???

  override def asString: String = ???
}

