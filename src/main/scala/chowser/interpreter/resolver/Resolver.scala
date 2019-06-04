package chowser.interpreter.resolver

import chowser.expressions.Sig.ScalarSig
import chowser.expressions.predef.ChowserPredef
import chowser.expressions.{Expression, Identifier, SymbolTable}
import chowser.interpreter.tokenize.Token
import chowser.interpreter.tokenize.Token.{IdentifierToken, WhiteSpaceToken}

object Resolver {

  def resolve(token: Token, namespace: Identifier, table: SymbolTable): Either[String, Expression] = {
    token match {
      case WhiteSpaceToken(_, _, _) => Left("This is only white space")
      case IdentifierToken(string, _, _) =>
        table.scalars.get(ScalarSig(namespace / string )).
          orElse(table.scalars.get(ScalarSig(ChowserPredef.namespace / string ))) match {
          case Some(scalarRef) => ??? // TODO
        }

    }
  }

}
