package chowser.expressions

import chowser.expressions.Expression.ArgId
import chowser.expressions.values.Value

case class ArgumentValues(list: ArgumentList, values: Map[ArgId, Value]) {

}

object ArgumentValues {
  def create(list: ArgumentList, valueList: Seq[Value]): ArgumentValues = {
    val values = list.argIds.zip(valueList).toMap
    ArgumentValues(list, values)
  }
}
