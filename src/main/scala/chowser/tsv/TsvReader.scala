package chowser.tsv

import chowser.tsv.BasicTsvReader.Row
import chowser.tsv.FilteredTsvReader.RowDoubleFilter

trait TsvReader extends Iterator[Row] {

  def filterByDoubleCol(colName: String, filter: Double => Boolean): FilteredTsvReader =
    new FilteredTsvReader(this, new RowDoubleFilter(colName, filter))

}
