package chowser.genomics

case class Location(chromosome: Chromosome, position: Int) extends Ordered[Location] {
  override def compare(that: Location): Int = {
    val chromosomeCompare = chromosome.compare(that.chromosome)
    if(chromosomeCompare != 0) {
      chromosomeCompare
    } else {
      position - that.position
    }
  }
}

