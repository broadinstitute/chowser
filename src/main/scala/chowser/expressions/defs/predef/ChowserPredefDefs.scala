package chowser.expressions.defs.predef

import chowser.expressions.defs.DefRegistry
import chowser.expressions.defs.DefRegistry.BasicDefRegistry
import chowser.expressions.defs.DefTable.BasicDefTable

object ChowserPredefDefs {



  val registry: DefRegistry = BasicDefRegistry(BasicDefTable.empty, BasicDefTable.empty, BasicDefTable.empty, BasicDefTable.empty)

}
