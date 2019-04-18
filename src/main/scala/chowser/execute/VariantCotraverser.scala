package chowser.execute

import better.files.File
import chowser.util.genomics.VariantIdLocation

object VariantCotraverser {

  def compare(file1: File, file2: File,
              fileToIter1: File => Iterator[VariantIdLocation], fileToIter2: File => Iterator[VariantIdLocation],
              fileBothOpt: Option[File], fileOnlyFirstOpt: Option[File], fileOnlySecondOpt: Option[File]): Unit = {


  }

}
