package chowser.expressions.tsv

import better.files.File
import chowser.expressions.Expression.{CallExpression, FloatLiteral}
import chowser.expressions.defs.SymbolTable
import chowser.expressions.defs.predef.ChowserPredefDefs
import chowser.expressions.predef.ChowserPredef
import chowser.expressions.{ChowserRuntime, Expression, ObjectType, Type}
import chowser.expressions.values.{BoolValue, ChowserObjectPool, LambdaValue, ObjectValue, Value}
import chowser.tsv.FilteredTsvReader.RowDoubleFilter
import chowser.tsv.{BasicTsvReader, TsvReader}

object TsvValues {

  case class TsvReaderObject(id: ObjectValue.Id)(val tsvReader: TsvReader) extends ObjectValue {
    override def tpe: Type = ChowserObjectPool.tsvReader
    def countRecords: Long = tsvReader.size
    def filterFloatCol(colName: String, lambdaValue: LambdaValue): TsvReaderObject = {
      val doubleFilter: Double => Boolean = { double =>
        CallExpression(FloatLiteral(double), lambdaValue.asExpression)
          .evaluate(new ChowserRuntime, ChowserPredefDefs.symbolTable) match {
          case Right(BoolValue(value)) => value
          case _ => false
        }
      }
      val filteredTsvReader = tsvReader.filterByDoubleCol(colName, doubleFilter)
      ChowserObjectPool.createInstance(TsvReaderObject(_)(filteredTsvReader))
    }
  }

  object TsvReaderObject {
    def create(file: File): TsvReaderObject = {
      val tsvReader = BasicTsvReader.forSimpleHeaderLine(file)
      ChowserObjectPool.createInstance(TsvReaderObject(_)(tsvReader))
    }
  }

}
