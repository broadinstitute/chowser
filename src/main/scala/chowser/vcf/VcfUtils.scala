package chowser.vcf

import chowser.execute.ChowserExecuter.Result
import chowser.util.io.{InputId, OutputId, ResourceConfig}
import org.broadinstitute.yootilz.core.snag.Snag

object VcfUtils {
  def transformVcf(inFile: InputId, outFile: OutputId, resourceConfig: ResourceConfig)(
    transformer: Iterator[VcfRecord] => Iterator[VcfRecord]): Either[Snag, Result] = {
    VcfReader(inFile, resourceConfig) match {
      case Right(reader) =>
        val writer = VcfWriter(outFile, reader.header, resourceConfig)
        transformer(reader.recordsIterator).foreach(writer.write)
        Right(Result.Done)
      case Left(snag) => Left(snag)
    }
  }

}
