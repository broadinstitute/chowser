package chowser.util.io

import org.broadinstitute.yootilz.core.snag.Snag

object IoIdUtils {

  def needsToBeFile[T](inputId: InputId)(fun: FileInputId => Either[Snag, T]): Either[Snag, T] = {
    inputId match {
      case fileInputId: FileInputId => fun(fileInputId)
      case _ => Left(Snag(s"This feature requires a local file, but $inputId is not"))
    }
  }

  def needsToBeFile[T](outputId: OutputId)(fun: FileOutputId => Either[Snag, T]): Either[Snag, T] = {
    outputId match {
      case fileOutputId: FileOutputId => fun(fileOutputId)
      case _ => Left(Snag(s"This feature requires a local file, but $outputId is not"))
    }
  }

}
