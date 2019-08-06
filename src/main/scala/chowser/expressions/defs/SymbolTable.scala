package chowser.expressions.defs

import chowser.expressions.defs.Def.{BinaryOpDef, FunctionDef, ScalarDef, UnitaryOpDef}
import chowser.expressions.defs.Ref.{BinaryOpRef, FunctionRef, ScalarRef, UnitaryOpRef}
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}
import chowser.expressions.defs.SymbolTable.{BinaryOpTable, FunctionTable, ScalarTable, UnitaryOpTable}

case class SymbolTable(scalarTable: ScalarTable,
                       unitaryOpTable: UnitaryOpTable,
                       binaryOpTable: BinaryOpTable,
                       functionTable: FunctionTable) {

  def +(scalarDef: ScalarDef): SymbolTable = copy(scalarTable = scalarTable + scalarDef)

  def +(unitaryOpDef: UnitaryOpDef): SymbolTable = copy(unitaryOpTable = unitaryOpTable + unitaryOpDef)

  def +(binaryOpDef: BinaryOpDef): SymbolTable = copy(binaryOpTable = binaryOpTable + binaryOpDef)

  def +(functionDef: FunctionDef): SymbolTable = copy(functionTable = functionTable + functionDef)
}

object SymbolTable {
  type ScalarTable = DefTable[ScalarSig, ScalarRef, ScalarDef]
  type UnitaryOpTable = DefTable[UnitaryOpSig, UnitaryOpRef, UnitaryOpDef]
  type BinaryOpTable = DefTable[BinaryOpSig, BinaryOpRef, BinaryOpDef]
  type FunctionTable = DefTable[FunctionSig, FunctionRef, FunctionDef]

  def empty: SymbolTable =
    SymbolTable(DefTable.empty, DefTable.empty, DefTable.empty, DefTable.empty)

}