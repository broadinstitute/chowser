package chowser.util.genomics

case class Location(chromosome: Chromosome, position: Int) {

}

object Location {
  object Ordering extends scala.math.Ordering[Location] {
    override def compare(location1: Location, location2: Location): Int = {
      val chromosomeComparison = Chromosome.Ordering.compare(location1.chromosome, location2.chromosome)
      if(chromosomeComparison != 0) {
        chromosomeComparison
      } else {
        location1.position - location2.position
      }
    }
  }
}
