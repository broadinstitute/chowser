package chowser.filter

import chowser.tsv.TsvReader

object RowFilters {

  case class ForCol(col: String, stringFilter: Filter[String]) extends Filter[TsvReader.Row] {
    override def apply(row: TsvReader.Row): Boolean = {
      row.valueMap.get(col) match {
        case Some(string) => stringFilter(string)
        case None => false
      }
    }
  }

}
