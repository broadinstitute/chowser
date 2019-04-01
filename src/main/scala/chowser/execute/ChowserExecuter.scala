package chowser.execute

import chowser.cmd.{ChowserCommand, TsvFilterCommand, VariantsRegionsCommand}

object ChowserExecuter {

  trait Result {
    def command: ChowserCommand
    def success: Boolean
  }

  def execute(command: ChowserCommand): Result = {
    command match {
      case tsvFilterCommand: TsvFilterCommand => TsvFilterExecuter.execute(tsvFilterCommand)
      case variantsRegionsCommand: VariantsRegionsCommand => VariantsRegionsExecuter.execute(variantsRegionsCommand)
    }
  }

}
