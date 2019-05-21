package chowser.expressions

import chowser.expressions.values.{IntValue, UnitValue}

trait Expression {
  def evaluate(context: Context): Result

}

object Exit extends Expression {
  override def evaluate(context: Context): Result = {
    context.exitIsRequested = true
    Success(UnitValue)
  }
}

case class IntLiteral(value: Long) extends Expression {
  override def evaluate(context: Context): Result = Success(IntValue(value))
}
