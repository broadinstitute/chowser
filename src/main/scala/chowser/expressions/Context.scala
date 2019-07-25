package chowser.expressions

import chowser.expressions.defs.SymbolTable
import chowser.expressions.defs.predef.ChowserPredefDefs

class Context(var symbolTable: SymbolTable) {
  var exitIsRequested: Boolean = false
}

object Context {
  def predef: Context = new Context(ChowserPredefDefs.registry)
}
