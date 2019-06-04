package chowser.expressions

import chowser.expressions.Ref.{BinaryOpRef, MethodRef, ScalarRef, UnitaryOpRef}
import chowser.expressions.Sig.{BinaryOpSig, MethodSig, ScalarSig, UnitaryOpSig}

case class SymbolTable(scalars: Map[ScalarSig, ScalarRef], unitaryOps: Map[UnitaryOpSig, UnitaryOpRef],
                       binaryOps: Map[BinaryOpSig, BinaryOpRef], methods: Map[MethodSig, MethodRef]) {

  def +(scalarRef: ScalarRef): SymbolTable = copy(scalars = scalars + (scalarRef.sig -> scalarRef))

  def +(unitaryOpRef: UnitaryOpRef): SymbolTable = copy(unitaryOps = unitaryOps + (unitaryOpRef.sig -> unitaryOpRef))

  def +(binaryOpRef: BinaryOpRef): SymbolTable = copy(binaryOps = binaryOps + (binaryOpRef.sig -> binaryOpRef))

  def +(methodRef: MethodRef): SymbolTable = copy(methods = methods + (methodRef.sig -> methodRef))
}
