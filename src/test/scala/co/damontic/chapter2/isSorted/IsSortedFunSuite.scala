import org.scalatest.FunSuite
import co.damontic.chapter2.isSorted.IsSorted

class IsSortedFunSuite extends FunSuite {

  val areIntsOrderedFunction: (Int, Int) => Boolean = {
    (a: Int, b: Int) =>
      {
        if (a < b) true
        else false
      }
  }

  val areStringsOrderedFunction: (String, String) => Boolean = {
    (a: String, b: String) =>
      {
        if (a < b) true
        else false
      }
  }

  test("given (1,2,3) and function that orders numbers return true") {
    val arg = Array(1, 2, 3)
    assert(IsSorted.isSorted(arg, areIntsOrderedFunction))
  }

  test("given (2,3,1) and function that orders numbers return false") {
    val arg = Array(2, 3, 1)
    assert(!IsSorted.isSorted(arg, areIntsOrderedFunction))
  }

  test("given ('angela','david','federico') and function that orders Strings return true") {
    val arg = Array("angela", "david", "federico")
    assert(IsSorted.isSorted(arg, areStringsOrderedFunction))
  }

  test("given ('david','angela', 'federico') and function that orders Strings return true") {
    val arg = Array( "david", "angela","federico")
    assert(!IsSorted.isSorted(arg, areStringsOrderedFunction))
  }

}
