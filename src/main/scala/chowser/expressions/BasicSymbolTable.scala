package chowser.expressions

import chowser.expressions.defs.Ref.{BinaryOpRef, FunctionRef, ScalarRef, UnitaryOpRef}
import chowser.expressions.defs.Sig.{BinaryOpSig, FunctionSig, ScalarSig, UnitaryOpSig}

case class BasicSymbolTable(scalars: Map[ScalarSig, ScalarRef], unitaryOps: Map[UnitaryOpSig, UnitaryOpRef],
                            binaryOps: Map[BinaryOpSig, BinaryOpRef], methods: Map[FunctionSig, FunctionRef]) {

  def +(scalarRef: ScalarRef): BasicSymbolTable = copy(scalars = scalars + (scalarRef.sig -> scalarRef))

  def +(unitaryOpRef: UnitaryOpRef): BasicSymbolTable = copy(unitaryOps = unitaryOps + (unitaryOpRef.sig -> unitaryOpRef))

  def +(binaryOpRef: BinaryOpRef): BasicSymbolTable = copy(binaryOps = binaryOps + (binaryOpRef.sig -> binaryOpRef))

  def +(methodRef: FunctionRef): BasicSymbolTable = copy(methods = methods + (methodRef.sig -> methodRef))
}
