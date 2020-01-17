package chowser.util.io

import java.io.{BufferedReader, InputStream, PrintWriter}
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets

import better.files.File
import com.google.auth.oauth2.{GoogleCredentials, ServiceAccountCredentials}
import com.google.cloud.storage.{BlobId, BlobInfo}
import com.google.cloud.{ReadChannel, WriteChannel}
import org.broadinstitute.yootilz.gcp.auth.OAuthUtils
import org.broadinstitute.yootilz.gcp.storage.GoogleStorageUtils

import scala.jdk.CollectionConverters._
import scala.util.Try

trait IoId {
  def asString: String

  override def toString: String = asString
}

trait InputId extends IoId {
  def newLineIterator(resourceConfig: ResourceConfig): Iterator[String]

  def newInputStream(resourceConfig: ResourceConfig): InputStream
}

object InputId {
  def apply(string: String): InputId = {
    GcpBlobId.parseBlobId(string).fold[InputId](FileInputId(File(string)))(GcpBlobInputId)
  }
}

trait OutputId extends IoId {
  def newPrintWriter(resourceConfig: ResourceConfig): PrintWriter
}

object OutputId {
  def apply(string: String): OutputId = {
    GcpBlobId.parseBlobId(string).fold[OutputId](FileOutputId(File(string)))(GcpBlobOutputId)
  }
}

trait FileIoId {
  def file: File

  def fileDeprecated: File = file
}

case class FileInputId(file: File) extends InputId with FileIoId {
  override def asString: String = file.toString()

  override def newLineIterator(resourceConfig: ResourceConfig): Iterator[String] = file.lineIterator

  override def newInputStream(resourceConfig: ResourceConfig): InputStream = file.newInputStream
}

case class FileOutputId(file: File) extends OutputId with FileIoId {
  override def asString: String = file.toString()

  override def newPrintWriter(resourceConfig: ResourceConfig): PrintWriter = file.newPrintWriter()
}

trait GcpBlobId extends IoId {
  def blobId: BlobId

  override def asString: String = blobId.toString

  protected def storageUtils(resourceConfig: ResourceConfig): GoogleStorageUtils = {
    val keyFileInputStreamOpt = resourceConfig.keyFileOpt.map(_.newInputStream(ResourceConfig.empty))
    val creds = keyFileInputStreamOpt.flatMap { serviceAccountIn =>
      Try(ServiceAccountCredentials.fromStream(serviceAccountIn)).toOption
    }.getOrElse(GoogleCredentials.getApplicationDefault).createScoped()
//    val credentials = OAuthUtils.getCredentials(keyFileInputStreamOpt)
    GoogleStorageUtils(creds, resourceConfig.gcpProjectOpt)
  }
}

object GcpBlobId {
  def parseBlobId(string: String): Option[BlobId] = {
    if(string.startsWith("gs://")) {
      val stringMinusPrefix = string.substring(5)
      val slashPos = stringMinusPrefix.indexOf('/')
      if(slashPos > 0 && slashPos < stringMinusPrefix.length - 1) {
        val bucketName = stringMinusPrefix.substring(0, slashPos)
        val objectName = stringMinusPrefix.substring(slashPos + 1)
        Some(BlobId.of(bucketName, objectName))
      } else {
        None
      }
    } else {
      None
    }
  }
}

case class GcpBlobInputId(blobId: BlobId) extends GcpBlobId with InputId {
  private def readChannel(resourceConfig: ResourceConfig): ReadChannel = storageUtils(resourceConfig).reader(blobId)

  override def newLineIterator(resourceConfig: ResourceConfig): Iterator[String] = {
    val reader = new BufferedReader(Channels.newReader(readChannel(resourceConfig), StandardCharsets.UTF_8))
    reader.lines().iterator().asScala
  }

  override def newInputStream(resourceConfig: ResourceConfig): InputStream = {
    Channels.newInputStream(readChannel(resourceConfig))
  }
}

case class GcpBlobOutputId(blobId: BlobId) extends GcpBlobId with OutputId {
  private def writeChannel(resourceConfig: ResourceConfig): WriteChannel = {
    storageUtils(resourceConfig).writerToNewBlob(blobId)
  }

  override def newPrintWriter(resourceConfig: ResourceConfig): PrintWriter = {
    new PrintWriter(Channels.newWriter(writeChannel(resourceConfig), StandardCharsets.UTF_8))
  }
}

