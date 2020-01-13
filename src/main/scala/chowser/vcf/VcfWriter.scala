package chowser.vcf

import java.io.PrintWriter

import chowser.util.io.{OutputId, ResourceConfig}

class VcfWriter(val writer: PrintWriter, val header: VcfHeader) {

  header.lines.foreach(writer.println)

  def write(record: VcfRecord): Unit = writer.println(record.toLine)

}

object VcfWriter {
  def apply(outputId: OutputId, header: VcfHeader, resourceConfig: ResourceConfig): VcfWriter =
    new VcfWriter(outputId.newPrintWriter(resourceConfig), header)
}
