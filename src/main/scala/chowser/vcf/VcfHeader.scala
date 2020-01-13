package chowser.vcf

import org.broadinstitute.yootilz.core.snag.Snag

case class VcfHeader(lines: Seq[String]) {

}

object VcfHeader {

  def fromLineIterator(lineIterator: Iterator[String]): Either[Snag, VcfHeader] = {
    var lines: Seq[String] = Seq.empty
    if (lineIterator.hasNext) {
      var line = lineIterator.next()
      lines :+= line
      while (line.startsWith("##")) {
        line = lineIterator.next()
        lines :+= line
      }
      if (line.startsWith("#")) {
        Right(VcfHeader(lines))
      } else {
        Left(Snag("Malformed VCF: header line missing."))
      }
    } else {
      Left(Snag("No lines to read."))
    }
  }
}
