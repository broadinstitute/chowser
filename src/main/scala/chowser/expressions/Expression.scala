package chowser.expressions

import chowser.expressions.values.{FloatValue, IntValue, StringValue, UnitValue}

trait Expression {
  def evaluate(context: Context): Result

}

object Expression {

  object Exit extends Expression {
    override def evaluate(context: Context): Result = {
      context.exitIsRequested = true
      Success(UnitValue)
    }
  }

  case class IntLiteral(value: Long) extends Expression {
    override def evaluate(context: Context): Result = Success(IntValue(value))
  }

  case class FloatLiteral(value: Double) extends Expression {
    override def evaluate(context: Context): Result = Success(FloatValue(value))
  }

  case class StringLiteral(value: String) extends Expression {
    override def evaluate(context: Context): Result = Success(StringValue(value))
  }

}
