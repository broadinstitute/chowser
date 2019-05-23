package chowser.vcf

case class VcfHeader(lines: Seq[String]) {

}

object VcfHeader {

  def fromLineIterator(lineIterator: Iterator[String]): Either[String, VcfHeader] = {
    var lines: Seq[String] = Seq.empty
    if(lineIterator.hasNext) {
      var line = lineIterator.next()
      lines += line
      while(line.startsWith("##")) {
        line = lineIterator.next()
        lines += line
      }
      if(line.startsWith("#")) {
        Right(VcfHeader(lines))
      } else {
        Left("Malformed VCF: header line missing,")
      }
    } else {
      Left("No lines to read.")
    }
  }
}
