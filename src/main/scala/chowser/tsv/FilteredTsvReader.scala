package chowser.tsv

import chowser.util.NumberParser

class FilteredTsvReader(reader: TsvReader, filter: TsvRow => Boolean) extends TsvReader {

  val filteredIter: Iterator[TsvRow] = reader.filter(filter)

  override def hasNext: Boolean = filteredIter.hasNext

  override def next(): TsvRow = filteredIter.next()
}

object FilteredTsvReader {
  class RowDoubleFilter(colName: String, filter: Double => Boolean) extends (TsvRow => Boolean) {
    override def apply(row: TsvRow): Boolean = {
      val string = row.string(colName)
      if(NumberParser.DoubleParser.isValid(string)) {
        filter(NumberParser.DoubleParser.parse(string))
      } else {
        false
      }
    }
  }
}