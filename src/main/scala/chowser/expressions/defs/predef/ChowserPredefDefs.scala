package chowser.expressions.defs.predef

import better.files.File
import chowser.expressions.Identifier
import chowser.expressions.defs.SymbolTable._
import chowser.expressions.defs.{DefTable, SymbolTable}
import chowser.expressions.tsv.TsvValues.TsvReader
import chowser.expressions.values.ValueOps.{floatBinary, intBinary, stringFunction1}

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

  val functionTable: FunctionTable = DefTable(Set(
    stringFunction1[TsvReader](Identifier(None, "TsvReader"),
      (fileString: String) => TsvReader.create(File(fileString)))
  ))

  val symbolTable: SymbolTable = SymbolTable(scalarTable, unitaryOpTable, binaryOpTable, functionTable)

}
