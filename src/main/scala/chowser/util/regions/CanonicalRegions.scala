package chowser.util.regions

import chowser.util.seqs.SeqUtils

case class CanonicalRegions(regions: Seq[Region]) {

  def :+(region: Region): CanonicalRegions = {
    if (regions.isEmpty) {
      CanonicalRegions.ofOne(region)
    } else {
      val mergeStart = SeqUtils.findFromWhatIndex(regions)(seqRegion => !seqRegion.isBeforeAndNonContiguousTo(region))
      val mergeEnd = SeqUtils.findFromWhatIndex(regions)(_.isAfterAndNonContiguousTo(region))
      val regionToInsert =
        if (mergeStart == mergeEnd) {
          region
        } else {
          Region.mergeAll(regions.slice(mergeStart, mergeEnd) :+ region)
        }
      CanonicalRegions(regions.patch(mergeStart, Seq(regionToInsert), mergeEnd - mergeStart))
    }
  }

}

object CanonicalRegions {
  def empty: CanonicalRegions = CanonicalRegions(Seq.empty)

  def ofOne(region: Region) = CanonicalRegions(Seq(region))
}
