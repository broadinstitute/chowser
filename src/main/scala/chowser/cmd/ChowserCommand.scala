package chowser.cmd

import chowser.util.io.{InputId, OutputId, ResourceConfig}

trait ChowserCommand {
  def resourceConfig: ResourceConfig
}

object ChowserCommand {

  trait OneInFile {
    def inFile: InputId
  }

  trait OneOutFile {
    def outFile: OutputId
  }

}
