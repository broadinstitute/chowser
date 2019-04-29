package chowser.genomics

import chowser.util.intervals.Interval

case class Region(chromosome: Chromosome, interval: Interval) {

  def start: Int = interval.start
  def end: Int = interval.end

  def includes(location: Location): Boolean =
    (chromosome == location.chromosome) && interval.contains(location.position)

}

object Region {
  def apply(chromosome: Chromosome, start: Int, end: Int): Region = Region(chromosome, Interval(start, end))
}
