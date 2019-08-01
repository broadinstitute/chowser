package chowser.filter

import chowser.tsv.BasicTsvReader

object RowFilters {

  case class ForCol(col: String, stringFilter: Filter[String]) extends Filter[BasicTsvReader.Row] {
    override def apply(row: BasicTsvReader.Row): Boolean = {
      row.valueMap.get(col) match {
        case Some(string) => stringFilter(string)
        case None => false
      }
    }
  }

}
