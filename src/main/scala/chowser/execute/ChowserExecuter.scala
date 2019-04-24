package chowser.execute

import chowser.cmd.{ChowserCommand, MatchVariantsCommand, TsvFilterCommand, TsvSortCommand, VariantsForRegionCommand, VariantsRegionsCommand}

trait ChowserExecuter[C <: ChowserCommand] {
  def execute(command: C): ChowserExecuter.Result[C]
}

object ChowserExecuter {

  trait Result[+C <: ChowserCommand] {
    def command: C
    def success: Boolean
  }

  def execute(command: ChowserCommand): Result[ChowserCommand] = {
    command match {
      case tsvFilterCommand: TsvFilterCommand => TsvFilterExecuter.execute(tsvFilterCommand)
      case tsvSortCommand: TsvSortCommand => TsvSortExecuter.execute(tsvSortCommand)
      case variantsRegionsCommand: VariantsRegionsCommand => VariantsRegionsExecuter.execute(variantsRegionsCommand)
      case variantsForRegionCommand: VariantsForRegionCommand =>
        VariantsForRegionExecuter.execute(variantsForRegionCommand)
      case compareVariantsCommand: MatchVariantsCommand => MatchVariantsExecuter.execute(compareVariantsCommand)
    }
  }

}
