package chowser.filter

import chowser.tsv.TsvIterator

object RowFilters {

  case class ForCol(col: String, stringFilter: Filter[String]) extends Filter[TsvIterator.Row] {
    override def apply(row: TsvIterator.Row): Boolean = {
      row.valueMap.get(col) match {
        case Some(string) => stringFilter(string)
        case None => false
      }
    }
  }

}
