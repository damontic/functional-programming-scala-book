package co.damontic.chapter3.list

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }
  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(head, tail) => head * product(tail)
  }
  def apply[A](elements : A*) : List[A] = {
    if(elements.isEmpty) Nil
    else Cons(elements.head, apply(elements.tail : _*))
  }
}
