package chowser.filter

object DoubleFilters {

  def all: Filter[Double] = Filter.all[Double]

  def none: Filter[Double] = Filter.none[Double]

  def greaterThan(limit: Double): Filter[Double] = (x: Double) => x > limit

  def lessThan(limit: Double): Filter[Double] = (x: Double) => x < limit

}
