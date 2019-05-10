package chowser.filter

import chowser.filter.Filter.{And, Not, Or}

trait Filter[-T] extends (T => Boolean) {

  def &&[TT <: T](oFilter: Filter[TT]): Filter[TT] = And(this, oFilter)

  def ||[TT <: T](oFilter: Filter[TT]): Filter[TT] = Or(this, oFilter)

  def unary_! : Filter[T] = Not[T](this)

}

object Filter {

  case class Equal[T](value: T) extends Filter[T] {
    override def apply(value: T): Boolean = value == this.value
  }

  class FilterAll[T] extends Filter[T] {
    override def apply(value: T): Boolean = true

    override def &&[TT <: T](oFilter: Filter[TT]): Filter[TT] = oFilter

    override def ||[TT <: T](oFilter: Filter[TT]): Filter[TT] = Filter.all[TT]

    override def unary_! : Filter[T] = Filter.none[T]
  }

  def all[T]: Filter[T] = new FilterAll[T]

  class FilterNone[T] extends Filter[T] {
    override def apply(value: T): Boolean = false

    override def &&[TT <: T](oFilter: Filter[TT]): Filter[TT] = Filter.none[TT]

    override def ||[TT <: T](oFilter: Filter[TT]): Filter[TT] = oFilter

    override def unary_! : Filter[T] = Filter.all[T]
  }

  def none[T]: Filter[T] = new FilterNone[T]

  case class And[-T](filters: Seq[Filter[T]]) extends Filter[T] {
    override def apply(value: T): Boolean = filters.forall(_.apply(value))

    override def &&[TT <: T](oFilter: Filter[TT]): Filter[TT] = {
      oFilter match {
        case _: FilterAll[T] => this
        case _: FilterNone[T] => Filter.none[TT]
        case oAnd: And[T] => And(filters ++ oAnd.filters)
        case _ => And(filters :+ oFilter)
      }
    }
  }

  object And {
    def apply[T](filter: Filter[T], filters: Filter[T]*): And[T] = And[T](filter +: filters)
  }

  case class Or[-T](filters: Seq[Filter[T]]) extends Filter[T] {
    override def apply(value: T): Boolean = filters.exists(_.apply(value))

    override def ||[TT <: T](oFilter: Filter[TT]): Filter[TT] = {
      oFilter match {
        case _: FilterAll[T] => Filter.all[TT]
        case _: FilterNone[T] => this
        case oOr: And[T] => And(filters ++ oOr.filters)
        case _ => Or(filters :+ oFilter)
      }
    }
  }

  object Or {
    def apply[T](filter: Filter[T], filters: Filter[T]*): Or[T] = Or[T](filter +: filters)
  }

  case class Not[-T](filter: Filter[T]) extends Filter[T] {
    override def apply(value: T): Boolean = !filter(value)

    override def unary_! : Filter[T] = filter
  }

}