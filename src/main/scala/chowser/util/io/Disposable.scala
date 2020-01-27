package chowser.util.io

import chowser.util.io.Disposable.Disposer

case class Disposable[A](a: A)(private val disposer: Disposer) {
  private var isDisposedVar: Boolean = false

  def get: A = a

  def isValid: Boolean = !isDisposedVar

  def isDisposed: Boolean = isDisposedVar

  def dispose(): Unit = {
    disposer.dispose()
    isDisposedVar = true
  }

  def useUp[B](user: A => B): B = {
    val b = user(a)
    disposer.dispose()
    b
  }

  def map[B](mapper: A => B): Disposable[B] = Disposable[B](mapper(a))(disposer)

  def map[B](mapper: A => Disposable[B]): Disposable[B] = ???
}

object Disposable {

  trait Disposer {
    def dispose(): Unit

    def ++ : Disposer
  }

  object Disposer {

    case class Composite(disposers: Seq[Disposer]) extends Disposer {
      override def dispose(): Unit = disposers.foreach(_.dispose())

      override def ++ : Disposer = ???
    }

  }

}