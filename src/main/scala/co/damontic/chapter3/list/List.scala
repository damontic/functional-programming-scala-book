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
  def tail[A]( list : List[A]) : List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }
  def setHead[A]( list : List[A], newElem : A) : List[A] = list match {
    case Nil => List(newElem)
    case Cons(head, tail) => Cons(newElem, tail)
  }
  def drop[A](list : List[A], n : Int) : List[A] = {
    def loop(counter : Int, newList : List[A]) : List[A] = {
      if(counter == n) List.tail(newList)
      else loop(counter+1, List.tail(newList))
    }
    loop(1, list)
  }
  def dropWhile[A](list : List[A], f : A => Boolean) : List[A] = list match {
    case Cons(h,t) => if(f(h)) dropWhile(t, f) else list
    case _ => list
  }
  def append[A](firstList : List[A], secondList : List[A]) : List[A] = firstList match {
    case Nil => secondList
    case Cons(h,t) => Cons(h , append(t, secondList))
  }
  def init[A](list : List[A]) : List[A] = {
    def revertList(aList: List[A]) : List[A] = {
      def loop(currentList : List[A], newList: List[A]) : List[A] = currentList match{
        case Nil => newList
        case Cons(h,t) => loop(t, Cons(h, newList))
      }
      loop(aList, Nil)
    }
    def loop(oldList : List[A], newList : List[A]) : List[A] = oldList match {
      case Nil => Nil
      case Cons(h, Nil) => newList
      case Cons(h,t) => loop(t, Cons(h, newList))
    }
    val rawList = loop(list, Nil)
    println(rawList)
    revertList(rawList)
  }
}
