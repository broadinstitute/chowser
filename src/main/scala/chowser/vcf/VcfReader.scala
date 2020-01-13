package chowser.vcf

import chowser.util.io.{InputId, ResourceConfig}
import org.broadinstitute.yootilz.core.snag.Snag

class VcfReader(val file: InputId, val header: VcfHeader, val recordsIterator: Iterator[VcfRecord]) {

}

object VcfReader {
  def apply(file: InputId, resourceConfig: ResourceConfig): Either[Snag, VcfReader] = {
    val lineIterator = file.newLineIterator(resourceConfig)
    VcfHeader.fromLineIterator(lineIterator) match {
      case Left(message) => Left(message)
      case Right(header) =>
        val recordsIterator = lineIterator.map(VcfRecord.fromLine)
        Right(new VcfReader(file, header, recordsIterator))
    }
  }
}