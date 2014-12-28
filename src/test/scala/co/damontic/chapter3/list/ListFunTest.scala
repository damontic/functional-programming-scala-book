package co.damontic.chapter3.list

import org.scalatest.FunSuite
import co.damontic.chapter3.list._

class ListFunTest extends FunSuite {

  test("List Sum(1,2)=3") {
    val lista = Cons(2, Cons(1, Nil))
    assert(List.sum(lista) === 3)
  }

  test("List product(1,2)=2") {
    val lista = Cons(2, Cons(1, Nil))
    assert(List.product(lista) === 2)
  }

  test("Pattern matching example") {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => 1
      case Nil => 2
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => 3
      case Cons(h,t) => 4
      case _ => 5
    }
    assert(x === 3)
  }

  test("List.tail(List(1,2,3,4,5))===List(2,3,4,5)") {
    val lista = List(1,2,3,4,5)
    assert(List.tail(lista) === List(2,3,4,5))
  }

  test("List.setHead(List(1,2,3,4,5), 8)===List(8,2,3,4,5)") {
    val lista = List(1,2,3,4,5)
    assert(List.setHead(lista, 8) === List(8,2,3,4,5))
  }

  test("List.drop(List(1,2,3,4,5), 3)===List(4,5)") {
    val lista = List(1,2,3,4,5)
    assert(List.drop(lista, 3) === List(4,5))
  }

  test("List.dropWhile(List(2,4,5,8), isPair)===List(5,8)") {
    val lista = List(2,4,5,8)
    val isPair : Int => Boolean = x => if( x % 2 == 0 ) true else false
    assert(List.dropWhile(lista, isPair) === List(5,8))
  }

  test("List.init(List(2,4,5,8))===List(2,4,5)") {
    val lista = List(2,4,5,8)
    assert(List.init(lista) === List(2,4,5))
  }
}
