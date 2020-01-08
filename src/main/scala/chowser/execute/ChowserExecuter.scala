package chowser.execute

import chowser.cmd._
import org.broadinstitute.yootilz.core.snag.Snag

trait ChowserExecuter[C <: ChowserCommand] {
  def execute(command: C): Either[Snag, ChowserExecuter.Result]
}

object ChowserExecuter extends ChowserExecuter[ChowserCommand] {

  trait Result {
  }

  object Result {
    object Done extends Result
  }

  override def execute(command: ChowserCommand): Either[Snag, Result] = {
    command match {
      case tsvMatrixCommand: TsvMatrixCommand => TsvMatrixExecuter.execute(tsvMatrixCommand)
      case tsvRangeCommand: TsvRangeCommand => TsvRangeExecuter.execute(tsvRangeCommand)
      case tsvSliceCommand: TsvSliceCommand => TsvSliceExecuter.execute(tsvSliceCommand)
      case tsvSortCommand: TsvSortCommand => TsvSortExecuter.execute(tsvSortCommand)
      case tsvSortIdsCommand: TsvSortIdsCommand => TsvSortIdsExecuter.execute(tsvSortIdsCommand)
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
      case variantsMatchTsvTsvCommand: VariantsMatchTsvTsvCommand =>
        VariantsMatchTsvTsvExecuter.execute(variantsMatchTsvTsvCommand)
      case variantsMatchVcfTsvCommand: VariantsMatchVcfTsvCommand =>
        VariantsMatchVcfTsvExecuter.execute(variantsMatchVcfTsvCommand)
      case variantsSelectTsvCommand: VariantsSelectTsvCommand =>
        VariantsSelectTsvExecuter.execute(variantsSelectTsvCommand)
      case variantsSelectVcfCommand: VariantsSelectVcfCommand =>
        VariantsSelectVcfExecuter.execute(variantsSelectVcfCommand)
      case caviarPToZCommand: CaviarPToZCommand => CaviarPToZExecuter.execute(caviarPToZCommand)
      case liftoverTsvCommand: LiftoverTsvCommand => LiftoverTsvExecuter.execute(liftoverTsvCommand)
      case ShellCommand => ShellExecuter.execute()
    }
  }

}
