import org.scalatest.FunSuite
import co.damontic.chapter2.compose.ComposeObject._

class ComposeFunSuite extends FunSuite {
  test("This tests the compose function") {
    val f : (Int => Int) = 2*_
    val g : (Int => Int) = _ + 2
    val z : (Int => Int) = 2*_ + 2
    val composeFG = compose(g,f)
    assert(z(3) == composeFG(3))
  }
}
