package chowser.expressions.tsv

import chowser.expressions.Expression.{CallExpression, FloatLiteral}
import chowser.expressions.defs.predef.ChowserPredefDefs
import chowser.expressions.values.{BoolValue, ChowserObjectPool, LambdaValue, ObjectValue}
import chowser.expressions.{ChowserRuntime, Type}
import chowser.tsv.{BasicTsvReader, TsvReader}
import chowser.util.io.{InputId, ResourceConfig}

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
    def create(file: InputId, resourceConfig: ResourceConfig): TsvReaderObject = {
      val tsvReader = BasicTsvReader.forSimpleHeaderLine(file, resourceConfig)
      ChowserObjectPool.createInstance(TsvReaderObject(_)(tsvReader))
    }
  }

}
