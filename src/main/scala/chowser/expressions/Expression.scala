package chowser.expressions

import java.util.UUID

import chowser.expressions.Expression.ParamExpression.ParamExpressionId
import chowser.expressions.defs.Sig.{BinaryOpSig, ScalarSig, UnitaryOpSig}
import chowser.expressions.defs.SymbolTable
import chowser.expressions.values._

trait Expression {
  def hasParameters: Boolean

  def createParameterList(argumentList: ParameterList): ParameterList

  def asString: String

  def withArgs(paramsToArgs: ParamsToArgs): Expression

  def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value]
}

object Expression {

  case class ParamId(uuid: UUID)

  object ParamId {
    def createNew: ParamId = ParamId(UUID.randomUUID())
  }

  object Exit extends Expression {
    def hasParameters: Boolean = false

    def createParameterList(mapping: ParameterList): ParameterList = mapping

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      runtime.exitIsRequested = true
      Right(UnitValue)
    }

    override def asString: String = "exit()"

    override def withArgs(paramsToArgs: ParamsToArgs): Expression = this
  }

  trait Literal extends Expression {
    def hasParameters: Boolean = false

    def createParameterList(mapping: ParameterList): ParameterList = mapping

    def asValue: Value

    def value: Any

    override def withArgs(paramsToArgs: ParamsToArgs): Literal

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Right[String, Value] = Right(asValue)

    override def asString: String = asValue.asString
  }

  case class IntLiteral(value: Long) extends Literal {
    override def asValue: IntValue = IntValue(value)

    override def withArgs(paramsToArgs: ParamsToArgs): IntLiteral = this
  }

  case class BoolLiteral(value: Boolean) extends Literal {
    override def asValue: BoolValue = BoolValue(value)

    override def withArgs(paramsToArgs: ParamsToArgs): BoolLiteral = this
  }

  case class FloatLiteral(value: Double) extends Literal {
    override def asValue: FloatValue = FloatValue(value)

    override def withArgs(paramsToArgs: ParamsToArgs): FloatLiteral = this
  }

  case class StringLiteral(value: String) extends Literal {
    override def asValue: StringValue = StringValue(value)

    override def withArgs(paramsToArgs: ParamsToArgs): StringLiteral = this
  }

  case class TypeLiteral(value: Type) extends Literal {
    override def asValue: Value = value

    override def withArgs(paramsToArgs: ParamsToArgs): TypeLiteral = this
  }

  object UnitLiteral extends Literal {
    override def value: Unit = ()

    override def asValue: UnitValue.type = UnitValue

    override def withArgs(paramsToArgs: ParamsToArgs): UnitLiteral.type = UnitLiteral
  }

  case class ObjectLiteral(objectValue: ObjectValue) extends Literal {
    override def asValue: ObjectValue = objectValue

    override def value: Any = objectValue

    override def withArgs(paramsToArgs: ParamsToArgs): ObjectLiteral = this
  }

  case class ParamExpression(id: ParamExpressionId)(val posOpt: Option[Int]) extends Expression {
    def hasParameters: Boolean = true

    override def createParameterList(mapping: ParameterList): ParameterList =
      mapping.withArgIdFor(this)

    override def asString: String = {
      posOpt match {
        case Some(pos) => s"_${pos}_"
        case None => "_"
      }
    }

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      Right(LambdaValue(this))
    }

    override def withArgs(paramsToArgs: ParamsToArgs): Expression = paramsToArgs.getArgForParam(id).getOrElse(this)
  }

  object ParamExpression {

    case class ParamExpressionId(uuid: UUID)

    object ParamExpressionId {
      def createNew(): ParamExpressionId = ParamExpressionId(UUID.randomUUID())
    }

    def createNewSliding(): ParamExpression = ParamExpression(ParamExpressionId.createNew())(None)

    def createNewPinned(pos: Int): ParamExpression = ParamExpression(ParamExpressionId.createNew())(Some(pos))
  }

  case class IdentifierExpression(identifier: Identifier) extends Expression {
    def hasParameters: Boolean = false

    def createParameterList(mapping: ParameterList): ParameterList = mapping

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

    override def withArgs(paramsToArgs: ParamsToArgs): IdentifierExpression = this
  }

  def asStringMaybeParenthesized(expression: Expression): String = {
    expression match {
      case literal: Literal => literal.asString
      case identifierExpression: IdentifierExpression => identifierExpression.asString
      case _ => "(" + expression.asString + ")"
    }
  }

  case class UnaryOpExpression(op: Operator, arg: Expression) extends Expression {
    def hasParameters: Boolean = arg.hasParameters

    def createParameterList(mapping: ParameterList): ParameterList = arg.createParameterList(mapping)

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      arg.evaluate(runtime, symbolTable) match {
        case failure: Left[String, Value] => failure
        case Right(argValue) =>
          argValue match {
            case LambdaValue(expressionInsideLambda) =>
              Right(LambdaValue(UnaryOpExpression(op, expressionInsideLambda)))
            case _ =>
              val opSig = UnitaryOpSig(Identifier(None, op.string), argValue.tpe)
              symbolTable.unitaryOpTable.lookupDef(opSig) match {
                case None => Left(s"No unary operator ${op.string} defined for argument type ${argValue.tpe}.")
                case Some(opDef) => opDef.function(argValue)
              }
          }
      }
    }

    override def asString: String = {
      val argString = asStringMaybeParenthesized(arg)
      op.string + argString
    }

    override def withArgs(paramsToArgs: ParamsToArgs): UnaryOpExpression =
      UnaryOpExpression(op, arg.withArgs(paramsToArgs))
  }

  case class BinaryOpExpression(op: Operator, lhs: Expression, rhs: Expression) extends Expression {
    def hasParameters: Boolean = lhs.hasParameters || rhs.hasParameters

    def createParameterList(argList: ParameterList): ParameterList = {
      rhs.createParameterList(lhs.createParameterList(argList))
    }

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      lhs.evaluate(runtime, symbolTable) match {
        case failure: Left[String, Value] => failure
        case Right(lhsValue) =>
          rhs.evaluate(runtime, symbolTable) match {
            case failure: Left[String, Value] => failure
            case Right(rhsValue) =>
              if (lhsValue.isLambdaValue || rhsValue.isLambdaValue) {
                Right(LambdaValue(BinaryOpExpression(op, lhsValue.asExpression, rhsValue.asExpression)))
              } else {
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
    }

    override def asString: String = {
      val lhsString = asStringMaybeParenthesized(lhs)
      val rhsString = asStringMaybeParenthesized(rhs)
      lhsString + op.string + rhsString
    }

    override def withArgs(paramsToArgs: ParamsToArgs): BinaryOpExpression =
      BinaryOpExpression(op, lhs.withArgs(paramsToArgs), rhs.withArgs(paramsToArgs))
  }

  def evaluateList(expressions: Seq[Expression], runtime: ChowserRuntime,
                   symbolTable: SymbolTable): Either[String, Seq[Value]] = {
    val expressionIter = expressions.iterator
    var failureOrValues: Either[String, Seq[Value]] = Right(Seq.empty)
    while (expressionIter.hasNext && failureOrValues.isRight) {
      expressionIter.next().evaluate(runtime, symbolTable) match {
        case Right(value) => failureOrValues = failureOrValues.map(_ :+ value)
        case Left(message) => failureOrValues = Left(message)
      }
    }
    failureOrValues
  }

  case class TupleExpression(elements: Seq[Expression]) extends Expression {
    def hasParameters: Boolean = elements.exists(_.hasParameters)

    override def createParameterList(argList: ParameterList): ParameterList = {
      var argListNew = argList
      for (element <- elements) {
        argListNew = element.createParameterList(argList)
      }
      argListNew
    }

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      evaluateList(elements, runtime, symbolTable) match {
        case Left(message) => Left(message)
        case Right(elementValues) =>
          if(elementValues.exists(_.isLambdaValue)) {
            Right(LambdaValue(TupleExpression(elementValues.map(_.asExpression))))
          } else {
            Right(TupleValue(elementValues))
          }
      }
    }

    override def asString: String = {
      elements.map(asStringMaybeParenthesized).mkString("(", ", ", ")")
    }

    override def withArgs(paramsToArgs: ParamsToArgs): TupleExpression =
      TupleExpression(elements.map(_.withArgs(paramsToArgs)))
  }

  case class CallExpression(arg: Expression, function: Expression) extends Expression {
    def hasParameters: Boolean = arg.hasParameters

    def args: Seq[Expression] = {
      arg match {
        case TupleExpression(elements) => elements
        case UnitLiteral => Seq()
        case _ => Seq(arg)
      }
    }

    override def createParameterList(argList: ParameterList): ParameterList = {
      var argListNew = argList
      for (arg <- args) {
        argListNew = arg.createParameterList(argList)
      }
      argListNew
    }

    override def evaluate(runtime: ChowserRuntime, symbolTable: SymbolTable): Either[String, Value] = {
      val paramList = function.createParameterList(ParameterList.empty)
      val paramsToArgs = ParamsToArgs.create(paramList, args)
      val functionResolved = function.withArgs(paramsToArgs)
      functionResolved.evaluate(runtime, symbolTable)
    }

    override def asString: String = {
      args.map(asStringMaybeParenthesized).mkString(" ") + " " + function.asString
    }

    override def withArgs(paramsToArgs: ParamsToArgs): CallExpression =
      CallExpression(arg.withArgs(paramsToArgs), function)
  }

}
