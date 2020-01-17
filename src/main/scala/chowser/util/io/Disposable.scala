package chowser.util.io

case class Disposable[A](a: A)(disposer: () => Unit) {
  private var isDisposedVar: Boolean = false

  def get: A = a

  def isValid: Boolean = !isDisposedVar

  def isDisposed: Boolean = isDisposedVar

  def dispose(): Unit = {
    disposer()
    isDisposedVar = true
  }

  def useUp[B](user: A => B): B = {
    val b = user(a)
    disposer()
    b
  }

  def map[B](mapper: A => B): Disposable[B] = Disposable[B](mapper(a))(disposer)
}
