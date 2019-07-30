package chowser.expressions

import chowser.expressions.Expression.ArgExpression.ArgExpressionId
import chowser.expressions.Expression.ArgId
import chowser.expressions.values.Value

case class ArgumentValues(list: ArgumentList, values: Map[ArgId, Value]) {
  def getValueForExpression(id: ArgExpressionId): Option[Value] = {
    list.argIdByExpression.get(id).flatMap(values.get)
  }
}

object ArgumentValues {
  def empty: ArgumentValues = ArgumentValues(ArgumentList.empty, Map.empty)

  def create(list: ArgumentList, valueList: Seq[Value]): ArgumentValues = {
    val values = list.argIds.zip(valueList).toMap
    ArgumentValues(list, values)
  }
}
