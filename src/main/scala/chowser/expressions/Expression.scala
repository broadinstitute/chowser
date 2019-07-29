package chowser.expressions

import java.util.UUID

import chowser.expressions.Expression.ArgExpression.ArgExpressionId
import chowser.expressions.Expression.ArgId
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}
import chowser.expressions.values._

trait Expression {
  def hasArguments: Boolean
  def createArgumentList(mapping: ArgumentList): ArgumentList
  final def argIds(context: Context): Seq[ArgId] = ???

  def asString: String

  def evaluate(context: Context): Result
}

object Expression {

  case class ArgId(uuid: UUID)

  object ArgId {
    def createNew: ArgId = ArgId(UUID.randomUUID())
  }

  object Exit extends Expression {
    def hasArguments: Boolean = false
    def createArgumentList(mapping: ArgumentList): ArgumentList = mapping
    override def evaluate(context: Context): Result = {
      context.exitIsRequested = true
      Success(UnitValue)
    }

    override def asString: String = "exit()"
  }

  trait Literal[T] extends Expression {
    def hasArguments: Boolean = false
    def createArgumentList(mapping: ArgumentList): ArgumentList = mapping
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

  case class ArgExpression(id: ArgExpressionId)(val posOpt: Option[Int]) extends Expression {
    def hasArguments: Boolean = true

    override def createArgumentList(mapping: ArgumentList): ArgumentList =
      mapping.withArgIdFor(this)
    override def asString: String = {
      posOpt match {
        case Some(pos) => s"_${pos}_"
        case None => "_`"
      }
    }

    override def evaluate(context: Context): Result = ???
  }

  object ArgExpression {
    case class ArgExpressionId(uuid: UUID)
    object ArgExpressionId {
      def createNew(): ArgExpressionId = ArgExpressionId(UUID.randomUUID())
    }
    def createNewSliding(): ArgExpression = ArgExpression(ArgExpressionId.createNew())(None)
    def createNewPinned(pos: Int): ArgExpression = ArgExpression(ArgExpressionId.createNew())(Some(pos))
  }

  case class IdentifierExpression(identifier: Identifier) extends Expression {
    def hasArguments: Boolean = false

    def createArgumentList(mapping: ArgumentList): ArgumentList = mapping

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
    def hasArguments: Boolean = arg.hasArguments
    def createArgumentList(mapping: ArgumentList): ArgumentList = arg.createArgumentList(mapping)
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
    def hasArguments: Boolean = lhs.hasArguments || rhs.hasArguments

    def createArgumentList(argList: ArgumentList): ArgumentList = {
      rhs.createArgumentList(lhs.createArgumentList(argList))
    }
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
    def hasArguments: Boolean = elements.exists(_.hasArguments)

    override def createArgumentList(argList: ArgumentList): ArgumentList = {
      var argListNew = argList
      for(element <- elements) {
        argListNew = element.createArgumentList(argList)
      }
      argListNew
    }

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
    def hasArguments: Boolean = args.exists(_.hasArguments)
    override def createArgumentList(argList: ArgumentList): ArgumentList = {
      var argListNew = argList
      for(arg <- args) {
        argListNew = arg.createArgumentList(argList)
      }
      argListNew
    }

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
