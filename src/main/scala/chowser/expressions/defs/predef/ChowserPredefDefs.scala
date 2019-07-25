package chowser.expressions.defs.predef

import chowser.expressions.defs.DefTable.BasicDefTable
import chowser.expressions.defs.SymbolTable
import chowser.expressions.defs.SymbolTable._
import chowser.expressions.values.ValueOps.{floatBinary, intBinary}

object ChowserPredefDefs {

  val scalarTable: BasicScalarTable = BasicDefTable.empty

  val unitaryOpTable: BasicUnitaryOpTable = BasicDefTable.empty

  val binaryOpTable: BasicBinaryOpTable = BasicDefTable(Set(
    intBinary("+", _ + _),
    intBinary("-", _ - _),
    intBinary("*", _ * _),
    intBinary("/", _ / _),
    floatBinary("+", _ + _),
    floatBinary("-", _ - _),
    floatBinary("*", _ * _),
    floatBinary("/", _ / _)
  ))

  val functionTable: BasicFunctionTable = BasicDefTable.empty

  val registry: SymbolTable = BasicSymbolTable(scalarTable, unitaryOpTable, binaryOpTable, functionTable)

}
