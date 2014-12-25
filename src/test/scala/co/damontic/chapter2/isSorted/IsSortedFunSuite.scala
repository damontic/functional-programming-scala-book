import org.scalatest.FunSuite
import co.damontic.chapter2.isSorted.IsSorted

class IsSortedFunSuite extends FunSuite {

  test("given (1,2,3) and function that orders numbers return true") {
    val arg = Array(1,2,3)
    val orderedFunction : (Int, Int) => Boolean = {
      (a: Int, b: Int) => {
        if(a < b) true
        else false
      }
    }
    assert(IsSorted.isSorted(arg, orderedFunction))
  }

  test("given (2,3,1) and function that orders numbers return false") {
    val arg = Array(2,3,1)
    val orderedFunction : (Int, Int) => Boolean = {
      (a: Int, b: Int) => {
        if(a < b) true
        else false
      }
    }
    assert(!IsSorted.isSorted(arg, orderedFunction))
  }

}
