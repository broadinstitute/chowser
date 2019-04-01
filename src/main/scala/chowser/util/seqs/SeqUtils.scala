package chowser.util.seqs

object SeqUtils {

  def findFromWhatIndex[A](seq: Seq[A])(predicate: A => Boolean): Int = {
    var indexMin = 0
    var indexMax = seq.size
    while(indexMin < indexMax) {
      if(indexMin + 1 == indexMax) {
        if(predicate(seq(indexMin))) {
          indexMax = indexMin
        } else {
          indexMin = indexMax
        }
      } else {
        val indexMid = (indexMin + indexMax) / 2
        if(predicate(seq(indexMid))) {
          indexMax = indexMid
        } else {
          indexMin = indexMid
        }
      }
    }
    indexMin
  }

}
