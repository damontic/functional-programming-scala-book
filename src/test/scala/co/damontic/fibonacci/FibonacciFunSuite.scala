import org.scalatest.FunSuite
import Fibonacci.fib

/**
 * Created by david on 1/11/14.
 */
class FibonacciFunSuite extends FunSuite {

  test("Fib(0)=0"){
    val result = fib(0)
    assert(result === 0)
  }

  test("Fib(1)=1"){
    val result = fib(1)
    assert(result === 1)
  }

  test("Fib(2)=1"){
    val result = fib(2)
    assert(result === 1)
  }

  test("Fib(3)=2"){
    val result = fib(3)
    assert(result === 2)
  }

  test("Fib(4)=3"){
    val result = fib(4)
    assert(result === 3)
  }

}
