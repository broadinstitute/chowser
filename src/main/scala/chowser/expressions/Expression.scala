package chowser.expressions

import chowser.expressions.defs.Sig.{BinaryOpSig, ScalarSig, UnitaryOpSig}
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

  trait Literal[T] extends Expression {
    def value: T
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

  case class IdentifierExpression(identifier: Identifier) extends Expression {
    override def evaluate(context: Context): Result = {
      val sig = ScalarSig(identifier)
      context.symbolTable.scalarTable.lookupRef(sig) match {
        case None => Failure(s"Unknown identifier ${identifier.asString}")
        case Some(_) => context.symbolTable.scalarTable.lookupDef(sig) match {
          case None => Failure(s"${identifier.asString} has not been initialized")
          case Some(myDef) => myDef.result
        }
      }
    }
  }

  case class UnaryOpExpression(op: Operator, arg: Expression) extends Expression {
    override def evaluate(context: Context): Result = {
      arg.evaluate(context) match {
        case failure: Failure => failure
        case Success(argValue) =>
          val opSig = UnitaryOpSig(Identifier(None, op.string), argValue.tpe)
          context.symbolTable.unitaryOpTable.lookupDef(opSig) match {
            case None => Failure(s"No unary operator ${op.string} defined for argument type ${argValue.tpe}.")
            case Some(opDef) => opDef.function(argValue)
          }
      }
    }
  }

  case class BinaryOpExpression(op: Operator, lhs: Expression, rhs: Expression) extends Expression {
    override def evaluate(context: Context): Result = {
      lhs.evaluate(context) match {
        case failure: Failure => failure
        case Success(lhsValue) =>
          rhs.evaluate(context) match {
            case failure: Failure => failure
            case Success(rhsValue) =>
              val opSig = BinaryOpSig(Identifier(None, op.string), lhsValue.tpe, rhsValue.tpe)
              context.symbolTable.binaryOpTable.lookupDef(opSig) match {
                case None =>
                  Failure(s"No binary operator ${op.string} defined for argument types" +
                    s" ${lhsValue.tpe.asString} and ${lhsValue.tpe.asString}.")
                case Some(opDef) => opDef.function(lhsValue, rhsValue)
              }
          }
      }
    }
  }



}
