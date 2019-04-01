package chowser.util.regions

case class Region(start: Int, end: Int) {

  def isContiguousWith(oRegion: Region): Boolean = (start <= oRegion.end) && (end >= oRegion.start)

  def isBeforeAndNonContiguousTo(oRegion: Region): Boolean = end < oRegion.start

  def isAfterAndNonContiguousTo(oRegion: Region): Boolean = oRegion.end < start

  def mergedWith(oRegion: Region): Region = Region(Math.min(start, oRegion.start), Math.max(end, oRegion.end))

}

object Region {
  def mergeAll(regions: Iterable[Region]): Region = {
    val starts = regions.map(_.start)
    val ends = regions.map(_.end)
    Region(starts.min, ends.max)
  }
}
