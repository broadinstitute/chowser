package chowser.expressions

import java.util.UUID

import chowser.expressions.Expression.ArgExpression.ArgExpressionId
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}
import chowser.expressions.defs.SymbolTable
import chowser.expressions.values._

trait Expression {
  def hasArguments: Boolean
  def createArgumentList(mapping: ArgumentList): ArgumentList

  def asString: String

  def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value]
}

object Expression {

  case class ArgId(uuid: UUID)

  object ArgId {
    def createNew: ArgId = ArgId(UUID.randomUUID())
  }

  object Exit extends Expression {
    def hasArguments: Boolean = false
    def createArgumentList(mapping: ArgumentList): ArgumentList = mapping
    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      runtime.exitIsRequested = true
      Right(UnitValue)
    }

    override def asString: String = "exit()"
  }

  trait Literal[T] extends Expression {
    def hasArguments: Boolean = false
    def createArgumentList(mapping: ArgumentList): ArgumentList = mapping
    def asValue: Value

    def value: T

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Right[String, Value] = Right(asValue)

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
        case None => "_"
      }
    }

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      symbolTable.argumentValues.getValueForExpression(id) match {
        case Some(value) => Right(value)
        case None => Right(LambdaValue(this, 1))
      }
    }
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

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      val sig = ScalarSig(identifier)
      symbolTable.scalarTable.lookupRef(sig) match {
        case None => Left(s"Unknown identifier ${identifier.asString}")
        case Some(_) => symbolTable.scalarTable.lookupDef(sig) match {
          case None => Left(s"${identifier.asString} has not been initialized")
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
    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      arg.evaluate(runtime, symbolTable) match {
        case failure: Left[String, Value] => failure
        case Right(argValue) =>
          val opSig = UnitaryOpSig(Identifier(None, op.string), argValue.tpe)
          symbolTable.unitaryOpTable.lookupDef(opSig) match {
            case None => Left(s"No unary operator ${op.string} defined for argument type ${argValue.tpe}.")
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
    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      lhs.evaluate(runtime, symbolTable) match {
        case failure: Left[String, Value] => failure
        case Right(lhsValue) =>
          rhs.evaluate(runtime, symbolTable) match {
            case failure: Left[String, Value] => failure
            case Right(rhsValue) =>
              val opSig = BinaryOpSig(Identifier(None, op.string), lhsValue.tpe, rhsValue.tpe)
              symbolTable.binaryOpTable.lookupDef(opSig) match {
                case None =>
                  Left(s"No binary operator ${op.string} defined for argument types" +
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

  def evaluateList(expressions: Seq[Expression], runtime: ChowserRuntime,
                   symbolTable: SymbolTable): Either[String, Seq[Value]] = {
    val expressionIter = expressions.iterator
    var failureOrValues: Either[String, Seq[Value]] = Right(Seq.empty)
    while(expressionIter.hasNext && failureOrValues.isRight) {
      expressionIter.next().evaluate(runtime, symbolTable) match {
        case Right(value) => failureOrValues = failureOrValues.map(_ :+ value)
        case Left(message) => failureOrValues = Left(message)
      }
    }
    failureOrValues
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

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      evaluateList(elements, runtime, symbolTable) match {
        case Left(message) => Left(message)
        case Right(elementValues) => Right(TupleValue(elementValues))
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

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      evaluateList(args, runtime, symbolTable) match {
        case Left(message) => Left(message)
        case Right(argValues) =>
          val argTypes = argValues.map(_.tpe)
          val funSig = FunctionSig(identifier, argTypes)
          val funRefOpt = symbolTable.functionTable.lookupDef(funSig)
          funRefOpt match {
            case None => Left(s"No function definition found for ${funSig}.")
            case Some(funDef) => funDef.function(argValues)
          }
      }
    }

    override def asString: String = {
      args.map(asStringMaybeParenthesized).mkString(" ") + " " + identifier.asString
    }
  }

}
