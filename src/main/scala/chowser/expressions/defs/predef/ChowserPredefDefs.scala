package chowser.expressions.defs.predef

import better.files.File
import chowser.expressions.{Identifier, IntType, StringType}
import chowser.expressions.defs.SymbolTable._
import chowser.expressions.defs.{DefTable, SymbolTable}
import chowser.expressions.tsv.TsvValues.TsvReaderObject
import chowser.expressions.values.{ChowserObjectPool, IntValue, LambdaValue}
import chowser.expressions.values.ValueOps.{floatBinary, floatCompare, intBinary, stringFunction1, tsvReaderColFilterFunction, tsvReaderFunction1}
import chowser.util.io.FileInputId

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
    floatBinary("/", _ / _),
    floatCompare("<", _ < _),
    floatCompare("<=", _ <= _),
    floatCompare(">", _ > _),
    floatCompare(">=", _ <= _),
    floatCompare("==", _ == _),
    floatCompare("!=", _  !=  _),
  ))

  val functionTable: FunctionTable = DefTable(Set(
    stringFunction1[TsvReaderObject](Identifier(None, "TsvReader"), ChowserObjectPool.tsvReader,
      (fileString: String) => TsvReaderObject.create(FileInputId(File(fileString)))),
    tsvReaderFunction1[IntValue](Identifier(None, "countRecords"), IntType,
      (tsvReader: TsvReaderObject) => IntValue(tsvReader.countRecords)),
    tsvReaderColFilterFunction(Identifier(None, "filterFloatCol"),
      (tsvReaderObject: TsvReaderObject, colName: String, filter: LambdaValue) =>
        tsvReaderObject.filterFloatCol(colName, filter))
  ))

  val symbolTable: SymbolTable = SymbolTable(scalarTable, unitaryOpTable, binaryOpTable, functionTable)

}
