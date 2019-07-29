package chowser.expressions

import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}
import chowser.expressions.values._

trait Expression {
  def asString: String
  def evaluate(context: Context): Result
}

object Expression {

  object Exit extends Expression {
    override def evaluate(context: Context): Result = {
      context.exitIsRequested = true
      Success(UnitValue)
    }

    override def asString: String = "exit()"
  }

  trait Literal[T] extends Expression {
    def asValue: Value
    def value: T
    override def evaluate(context: Context): Result = Success(asValue)
    override def asString: String = asValue.asString
  }

  case class IntLiteral(value: Long) extends Literal[Long] {
    override def asValue: IntValue = IntValue(value)
  }

  case class FloatLiteral(value: Double) extends Literal[Double] {
    override def asValue: FloatValue = FloatValue(value)
  }

  case class StringLiteral(value: String) extends Literal[String] {
    override def asValue: StringValue = StringValue(value)
  }

  object UnitLiteral extends Literal[Unit] {
    override def value: Unit = ()
    override def asValue: UnitValue.type = UnitValue
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

    override def asString: String = identifier.asString
  }

  def asStringMaybeParenthesized(expression: Expression): String = {
    expression match {
      case literal: Literal[_] => literal.asString
      case identifierExpression: IdentifierExpression => identifierExpression.asString
      case _ => "(" + expression.asString + ")"
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

    override def asString: String = {
      val argString = asStringMaybeParenthesized(arg)
      op.string + argString
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
                    s" ${lhsValue.tpe.asString} and ${rhsValue.tpe.asString}.")
                case Some(opDef) => opDef.function(lhsValue, rhsValue)
              }
          }
      }
    }

    override def asString: String = {
      val lhsString = asStringMaybeParenthesized(lhs)
      val rhsString = asStringMaybeParenthesized(lhs)
      lhsString + op.string + rhsString
    }
  }

  case class TupleExpression(elements: Seq[Expression]) extends Expression {
    override def evaluate(context: Context): Result = {
      val elementIter = elements.iterator
      var values: Seq[Value] = Seq.empty
      var failureOpt: Option[Failure] = None
      while (failureOpt.isEmpty && elementIter.hasNext) {
        elementIter.next().evaluate(context) match {
          case Success(value) => values :+= value
          case failure: Failure => failureOpt = Some(failure)
        }
      }
      failureOpt match {
        case Some(failure) => failure
        case None => Success(TupleValue(values))
      }
    }

    override def asString: String = {
      elements.map(asStringMaybeParenthesized).mkString("(", ", ", ")")
    }
  }

  case class CallExpression(args: Seq[Expression], identifier: Identifier) extends Expression {
    override def evaluate(context: Context): Result = {
      val argIter = args.iterator
      var argValues: Seq[Value] = Seq.empty
      var failureOpt: Option[Failure] = None
      while (failureOpt.isEmpty && argIter.hasNext) {
        argIter.next().evaluate(context) match {
          case Success(value) => argValues :+= value
          case failure: Failure => failureOpt = Some(failure)
        }
      }
      failureOpt match {
        case Some(failure) => failure
        case None =>
          val argTypes = argValues.map(_.tpe)
          val funSig = FunctionSig(identifier, argTypes)
          val funRefOpt = context.symbolTable.functionTable.lookupDef(funSig)
          funRefOpt match {
            case None => Failure(s"No function definition found for ${funSig}.")
            case Some(funDef) => funDef.function(argValues)
          }
      }
    }

    override def asString: String = {
      args.map(asStringMaybeParenthesized).mkString(" ") + " " + identifier.asString
    }
  }

}
