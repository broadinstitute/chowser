package chowser.expressions

import chowser.expressions.values.{FloatValue, IntValue, StringValue, UnitValue}

trait Expression {
  def mayHaveEffects: Boolean
  def evaluate(context: Context): Result

}

object Expression {

  object Exit extends Expression {
    override def evaluate(context: Context): Result = {
      context.exitIsRequested = true
      Success(UnitValue)
    }

    override def mayHaveEffects: Boolean = true
  }

  trait Literal[T] extends Expression {
    def value: T
    override def mayHaveEffects: Boolean = false
  }

  case class IntLiteral(value: Long) extends Literal[Long] {
    override def evaluate(context: Context): Result = Success(IntValue(value))
  }

  case class FloatLiteral(value: Double) extends Literal[Double] {
    override def evaluate(context: Context): Result = Success(FloatValue(value))
  }

  case class StringLiteral(value: String) extends Literal[String] {
    override def evaluate(context: Context): Result = Success(StringValue(value))
  }

}
