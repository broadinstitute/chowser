package chowser.expressions.defs.predef

import chowser.expressions.defs.SymbolTable._
import chowser.expressions.defs.{DefTable, SymbolTable}
import chowser.expressions.values.ValueOps.{floatBinary, intBinary}

object ChowserPredefDefs {

  val scalarTable: ScalarTable = DefTable.empty

  val unitaryOpTable: UnitaryOpTable = DefTable.empty

  val binaryOpTable: BinaryOpTable = DefTable(Set(
    intBinary("+", _ + _),
    intBinary("-", _ - _),
    intBinary("*", _ * _),
    intBinary("/", _ / _),
    floatBinary("+", _ + _),
    floatBinary("-", _ - _),
    floatBinary("*", _ * _),
    floatBinary("/", _ / _)
  ))

  val functionTable: FunctionTable = DefTable.empty

  val symbolTable: SymbolTable = SymbolTable(scalarTable, unitaryOpTable, binaryOpTable, functionTable)

}
