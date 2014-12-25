import org.scalatest.FunSuite
import co.damontic.chapter2.currying.CurryingObject._

class CurryingObjectFunSuite extends FunSuite {

  test("test currying") {
    val sum = (x: Int, y: Int) => x+y 
    val curriedSum = curry(sum)
    assert(sum(2,3) == curriedSum(2)(3))
    val uncurriedSum = uncurry(curriedSum)
    assert(uncurriedSum(2,3) == sum(2,3))
  }

}
