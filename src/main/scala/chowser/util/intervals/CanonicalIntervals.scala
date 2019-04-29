package chowser.util.intervals

import chowser.util.seqs.SeqUtils

case class CanonicalIntervals(intervals: Seq[Interval]) {

  def :+(interval: Interval): CanonicalIntervals = {
    if (intervals.isEmpty) {
      CanonicalIntervals.ofOne(interval)
    } else {
      val mergeStart =
        SeqUtils.findFromWhatIndex(intervals)(seqInterval => !seqInterval.isBeforeAndNonContiguousTo(interval))
      val mergeEnd = SeqUtils.findFromWhatIndex(intervals)(_.isAfterAndNonContiguousTo(interval))
      val intervalToInsert =
        if (mergeStart == mergeEnd) {
          interval
        } else {
          Interval.mergeAll(intervals.slice(mergeStart, mergeEnd) :+ interval)
        }
      CanonicalIntervals(intervals.patch(mergeStart, Seq(intervalToInsert), mergeEnd - mergeStart))
    }
  }

}

object CanonicalIntervals {
  def empty: CanonicalIntervals = CanonicalIntervals(Seq.empty)

  def ofOne(region: Interval) = CanonicalIntervals(Seq(region))
}
