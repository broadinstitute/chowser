package chowser.cmd

import better.files.File

case class VariantsSelectVcfCommand(dataFile: File, selectionFile: File, outFile: File, idColSelection: String)
  extends ChowserCommand {

}
