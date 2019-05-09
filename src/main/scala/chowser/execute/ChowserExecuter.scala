package chowser.execute

import chowser.cmd.{ChowserCommand, VariantsMatchVcfTsvCommand, TsvExtractUniqueCommand, TsvRangeCommand, TsvSliceCommand, TsvSortCommand, VariantsCanonicalizeTsvCommand, VariantsCanonicalizeVcfCommand, VariantsForRegionByIdCommand, VariantsForRegionCommand, VariantsRegionsCommand}

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
      case tsvRangeCommand: TsvRangeCommand => TsvRangeExecuter.execute(tsvRangeCommand)
      case tsvSliceCommand: TsvSliceCommand => TsvSliceExecuter.execute(tsvSliceCommand)
      case tsvSortCommand: TsvSortCommand => TsvSortExecuter.execute(tsvSortCommand)
      case tsvExtractUniqueCommand: TsvExtractUniqueCommand =>
        TsvExtractUniqueExecuter.execute(tsvExtractUniqueCommand)
      case variantsCanonicalizeVcfCommand: VariantsCanonicalizeVcfCommand =>
        VariantsCanonicalizeVcfExecuter.execute(variantsCanonicalizeVcfCommand)
      case variantsCanonicalizeTsvCommand: VariantsCanonicalizeTsvCommand =>
        VariantsCanonicalizeTsvExecuter.execute(variantsCanonicalizeTsvCommand)
      case variantsRegionsCommand: VariantsRegionsCommand => VariantsRegionsExecuter.execute(variantsRegionsCommand)
      case variantsForRegionCommand: VariantsForRegionCommand =>
        VariantsForRegionExecuter.execute(variantsForRegionCommand)
      case variantsForRegionByIdCommand: VariantsForRegionByIdCommand =>
        VariantsForRegionByIdExecuter.execute(variantsForRegionByIdCommand)
      case compareVariantsCommand: VariantsMatchVcfTsvCommand => VariantsMatchVcfTsvExecuter.execute(compareVariantsCommand)
    }
  }

}
