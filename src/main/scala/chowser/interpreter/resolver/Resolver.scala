package chowser.interpreter.resolver

import chowser.expressions.defs.DefRegistry
import chowser.expressions.defs.Sig.ScalarSig
import chowser.expressions.predef.ChowserPredef
import chowser.expressions.{Failure, Identifier, Result}
import chowser.interpreter.tokenize.Token
import chowser.interpreter.tokenize.Token.{IdentifierToken, WhiteSpaceToken}

object Resolver {

  def resolve(token: Token, namespace: Identifier, registry: DefRegistry): Result = {
    token match {
      case WhiteSpaceToken(_, _, _) => Failure("This is only white space")
      case IdentifierToken(string, _, _) =>
        registry.scalarTable.lookupDef(ScalarSig(namespace / string )).
          orElse(registry.scalarTable.lookupDef(ScalarSig(ChowserPredef.namespace / string ))) match {
          case Some(scalarDef) => scalarDef.result
          case None => Failure(s"Unknown identifier $string")
        }
      case _ => Failure(s"Not implemented yet for ${token.string}.")

    }
  }

}
