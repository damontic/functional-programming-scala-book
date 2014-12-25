package co.damontic.chapter2.compose

object ComposeObject {
  def compose[A,B,C](f: B => C, g: A => B) : A => C = {
    a: A => {
      f(g(a))
    }
  }
}
