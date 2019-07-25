package chowser.expressions

import chowser.expressions.defs.DefRegistry

class Context {
  var exitIsRequested: Boolean = false
  var symbolTable: DefRegistry = DefRegistry.empty
}
