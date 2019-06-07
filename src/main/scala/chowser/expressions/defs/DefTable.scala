package chowser.expressions.defs

import chowser.expressions.defs.DefTable.CombinedDefTable

trait DefTable[S <: Sig, R <: Ref[S], D <: Def[S, R]] {
  def lookupRef(sig: S): Option[R]

  def lookupDef(sig: S): Option[D]

  def ++(that: DefTable[S, R, D]): CombinedDefTable[S, R, D] = CombinedDefTable(this, that)
}

object DefTable {

  case class BasicDefTable[S <: Sig, R <: Ref[S], D <: Def[S, R]](refs: Map[S, R], defs: Map[S, D])
    extends DefTable[S, R, D] {
    def +(d: D): BasicDefTable[S, R, D] = BasicDefTable(refs + (d.ref.sig -> d.ref), defs + (d.ref.sig -> d))

    override def lookupRef(sig: S): Option[R] = refs.get(sig)

    override def lookupDef(sig: S): Option[D] = defs.get(sig)
  }

  object BasicDefTable {
    def empty[S <: Sig, R <: Ref[S], D <: Def[S, R]]: BasicDefTable[S, R, D] = BasicDefTable(Map.empty, Map.empty)

    def apply[S <: Sig, R <: Ref[S], D <: Def[S, R]](defs: Set[D]): BasicDefTable[S, R, D] = {
      BasicDefTable(defs.map(d => d.sig -> d.ref).toMap, defs.map(d => d.sig -> d).toMap)
    }
  }

  case class CombinedDefTable[S <: Sig, R <: Ref[S], D <: Def[S, R]](table1: DefTable[S, R, D],
                                                                     table2: DefTable[S, R, D])
    extends DefTable[S, R, D] {
    override def lookupRef(sig: S): Option[R] = table1.lookupRef(sig).orElse(table2.lookupRef(sig))

    override def lookupDef(sig: S): Option[D] = table1.lookupDef(sig).orElse(table2.lookupDef(sig))
  }

}
