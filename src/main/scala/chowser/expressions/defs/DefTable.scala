package chowser.expressions.defs

case class DefTable[S <: Sig, R <: Ref[S], D <: Def[S, R]](refs: Map[S, R], defs: Map[S, D]) {
  def +(d: D): DefTable[S, R, D] = DefTable(refs + (d.ref.sig -> d.ref), defs + (d.ref.sig -> d))

  def lookupRef(sig: S): Option[R] = refs.get(sig)

  def lookupDef(sig: S): Option[D] = defs.get(sig)
}

object DefTable {
  def empty[S <: Sig, R <: Ref[S], D <: Def[S, R]]: DefTable[S, R, D] = DefTable(Map.empty, Map.empty)

  def apply[S <: Sig, R <: Ref[S], D <: Def[S, R]](defs: Set[D]): DefTable[S, R, D] = {
    DefTable(defs.map(d => d.sig -> d.ref).toMap, defs.map(d => d.sig -> d).toMap)
  }
}
