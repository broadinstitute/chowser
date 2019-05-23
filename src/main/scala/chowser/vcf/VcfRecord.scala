package chowser.vcf

import chowser.genomics.VariantGroupId

case class VcfRecord(chrom: String, pos: String, id: String, ref: String, alt: String, qual: String, filter: String,
                     info: String, format: String, others: Seq[String]) {

  def getCanonicalId: Either[String, VariantGroupId] = VariantGroupId.parse(chrom, pos, ref, alt)

  def withId(id: String): VcfRecord = copy(id = id)

  def withCanonicalId: VcfRecord = {
    getCanonicalId match {
      case Right(canonicalId) => withId(canonicalId.toString)
      case Left(message) =>
        println(message)
        this
    }
  }

  def toLine: String = (Seq(chrom, pos, id, ref, alt, qual, filter, info, format) ++ others).mkString("\t")
}

object VcfRecord {
  def fromLine(line: String): VcfRecord = {
    val fields = line.split("\t")
    val chrom = fields(0)
    val pos = fields(1)
    val id = fields(2)
    val ref = fields(3)
    val alt = fields(4)
    val qual = fields(5)
    val filter = fields(6)
    val info = fields(7)
    val format = fields(8)
    val others = fields.drop(9)
    VcfRecord(chrom, pos, id, ref, alt, qual, filter, info, format, others)
  }
}
