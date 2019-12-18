package chowser.filter

import chowser.tsv.{BasicTsvReader, TsvRow}

object RowFilters {

  case class ForCol(col: String, stringFilter: Filter[String]) extends Filter[TsvRow] {
    override def apply(row: TsvRow): Boolean = {
      row.valueMap.get(col) match {
        case Some(string) => stringFilter(string)
        case None => false
      }
    }
  }

}
