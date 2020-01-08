package chowser.cmd

import chowser.util.io.IoId

trait ChowserCommand {

}

object ChowserCommand {

  trait OneInFile {
    def inFile: IoId
  }

  trait OneOutFile {
    def outFile: IoId
  }

}
