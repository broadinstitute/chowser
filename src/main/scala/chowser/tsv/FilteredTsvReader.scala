package chowser.tsv

import chowser.tsv.BasicTsvReader.Row
import chowser.util.NumberParser

class FilteredTsvReader(reader: TsvReader, filter: Row => Boolean) extends TsvReader {

  val filteredIter = reader.filter(filter)

  override def hasNext: Boolean = filteredIter.hasNext

  override def next(): Row = filteredIter.next()
}

object FilteredTsvReader {
  class RowDoubleFilter(colName: String, filter: Double => Boolean) extends (Row => Boolean) {
    override def apply(row: Row): Boolean = {
      val string = row.string(colName)
      if(NumberParser.DoubleParser.isValid(string)) {
        filter(NumberParser.DoubleParser.parse(string))
      } else {
        false
      }
    }
  }
}