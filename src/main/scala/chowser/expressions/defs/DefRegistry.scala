package chowser.expressions.defs

import chowser.expressions.defs.Def.{BinaryOpDef, FunctionDef, ScalarDef, UnitaryOpDef}
import chowser.expressions.defs.DefRegistry.{BinaryOpTable, FunctionTable, ScalarTable, UnitaryOpTable}
import chowser.expressions.defs.DefTable.BasicDefTable
import chowser.expressions.defs.Ref.{BinaryOpRef, FunctionRef, ScalarRef, UnitaryOpRef}
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}

trait DefRegistry {
  def scalarTable: ScalarTable

  def unitaryOpTable: UnitaryOpTable

  def binaryOpTable: BinaryOpTable

  def functionTable: FunctionTable
}

object DefRegistry {
  type ScalarTable = DefTable[ScalarSig, ScalarRef, ScalarDef]
  type UnitaryOpTable = DefTable[UnitaryOpSig, UnitaryOpRef, UnitaryOpDef]
  type BinaryOpTable = DefTable[BinaryOpSig, BinaryOpRef, BinaryOpDef]
  type FunctionTable = DefTable[FunctionSig, FunctionRef, FunctionDef]

  type BasicScalarTable = BasicDefTable[ScalarSig, ScalarRef, ScalarDef]
  type BasicUnitaryOpTable = BasicDefTable[UnitaryOpSig, UnitaryOpRef, UnitaryOpDef]
  type BasicBinaryOpTable = BasicDefTable[BinaryOpSig, BinaryOpRef, BinaryOpDef]
  type BasicFunctionTable = BasicDefTable[FunctionSig, FunctionRef, FunctionDef]

  def empty: BasicDefRegistry =
    BasicDefRegistry(BasicDefTable.empty, BasicDefTable.empty, BasicDefTable.empty, BasicDefTable.empty)

  case class BasicDefRegistry(scalarTable: BasicScalarTable,
                              unitaryOpTable: BasicUnitaryOpTable,
                              binaryOpTable: BasicBinaryOpTable,
                              functionTable: BasicFunctionTable) extends DefRegistry {
    def +(scalarDef: ScalarDef): BasicDefRegistry = copy(scalarTable = scalarTable + scalarDef)

    def +(unitaryOpDef: UnitaryOpDef): BasicDefRegistry = copy(unitaryOpTable = unitaryOpTable + unitaryOpDef)

    def +(binaryOpDef: BinaryOpDef): BasicDefRegistry = copy(binaryOpTable = binaryOpTable + binaryOpDef)

    def +(functionDef: FunctionDef): BasicDefRegistry = copy(functionTable = functionTable + functionDef)
  }

  case class CombinedDefRegistry(registry1: DefRegistry, registry2: DefRegistry) extends DefRegistry {
    override val scalarTable: ScalarTable = registry1.scalarTable ++ registry2.scalarTable

    override def unitaryOpTable: UnitaryOpTable = registry1.unitaryOpTable ++ registry2.unitaryOpTable

    override def binaryOpTable: BinaryOpTable = registry1.binaryOpTable ++ registry2.binaryOpTable

    override def functionTable: FunctionTable = registry1.functionTable ++ registry2.functionTable
  }

}