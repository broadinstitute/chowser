package chowser.expressions.defs.predef

import chowser.expressions.defs.Def.BinaryOpDef
import chowser.expressions.defs.SymbolTable
import chowser.expressions.defs.SymbolTable.{BasicBinaryOpTable, BasicFunctionTable, BasicScalarTable, BasicSymbolTable, BasicUnitaryOpTable}
import chowser.expressions.defs.DefTable.BasicDefTable
import chowser.expressions.defs.Ref.BinaryOpRef
import chowser.expressions.defs.Sig.BinaryOpSig
import chowser.expressions.values.ValueOps
import chowser.expressions.{Identifier, IntType, Type}

object ChowserPredefDefs {

  val scalarTable: BasicScalarTable = BasicDefTable.empty

  val unitaryOpTable: BasicUnitaryOpTable = BasicDefTable.empty

  val binaryOpTable: BasicBinaryOpTable = BasicDefTable(Set(
    BinaryOpDef(BinaryOpRef(BinaryOpSig(Identifier(None, "+"), IntType, IntType), IntType), ValueOps.intBinary(_+_))
  ))

  val functionTable: BasicFunctionTable = BasicDefTable.empty

  val registry: SymbolTable = BasicSymbolTable(scalarTable, unitaryOpTable, binaryOpTable, functionTable)

}
