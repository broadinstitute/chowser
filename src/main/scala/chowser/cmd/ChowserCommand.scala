package chowser.cmd

import better.files.File

trait ChowserCommand {

}

object ChowserCommand {

  trait OneInFile {
    def inFile: File
  }

  trait OneOutFile {
    def outFile: File
  }

}
