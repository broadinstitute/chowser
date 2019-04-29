package chowser.util.intervals

case class Interval(start: Int, end: Int) {

  def contains(position: Int): Boolean = position >= start && position < end

  def isContiguousWith(oInterval: Interval): Boolean = (start <= oInterval.end) && (end >= oInterval.start)

  def isBeforeAndNonContiguousTo(oInterval: Interval): Boolean = end < oInterval.start

  def isAfterAndNonContiguousTo(oInterval: Interval): Boolean = oInterval.end < start

  def mergedWith(oInterval: Interval): Interval =
    Interval(Math.min(start, oInterval.start), Math.max(end, oInterval.end))

}

object Interval {
  def mergeAll(intervals: Iterable[Interval]): Interval = {
    val starts = intervals.map(_.start)
    val ends = intervals.map(_.end)
    Interval(starts.min, ends.max)
  }
}
