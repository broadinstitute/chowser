package chowser.cmd

import better.files.File

case class VariantsSelectTsvCommand(dataFile: File, selectionFile: File, outFile: File,
                                    idColData: String, idColSelection: String)
  extends ChowserCommand {

}
