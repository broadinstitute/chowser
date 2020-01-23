package chowser.util

object IntersecterAndDiffer {

  def intersectAndDiffer[V](set1: Set[V], set2: Set[V]): IntersectionAndDiffs[V] = {
    val intersection = set1.intersect(set2)
    val diff1 = set1 -- set2
    val diff2 = set2 -- set1
    IntersectionAndDiffs[V](intersection, diff1, diff2)
  }

  def intersectAndDiffByKey[K, V](set1: Set[V], set2: Set[V], toKey: V => K): IntersectionAndDiffs[V] = {
    def toMap(set: Set[V]): Map[K, V] = set.map(value => (toKey(value), value)).toMap
    val map1 =toMap(set1)
    val map2 = toMap(set2)
    val keys1 = map1.keySet
    val keys2 = map2.keySet
    val commonKeys = keys1.intersect(keys2)
    val diff1 = (map1 -- keys2).values.toSet
    val diff2 = (map2 -- keys1).values.toSet
    val intersection = commonKeys.flatMap(key => map2.get(key))
    IntersectionAndDiffs(intersection, diff1, diff2)
  }

  case class IntersectionAndDiffs[V](intersection: Set[V], diff1: Set[V], diff2: Set[V])

}
