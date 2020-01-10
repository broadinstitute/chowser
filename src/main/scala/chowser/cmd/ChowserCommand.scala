package chowser.cmd

import chowser.util.io.{InputId, OutputId}

trait ChowserCommand {

}

object ChowserCommand {

  trait OneInFile {
    def inFile: InputId
  }

  trait OneOutFile {
    def outFile: OutputId
  }

}
