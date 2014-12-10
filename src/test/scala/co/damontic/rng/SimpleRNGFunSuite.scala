package co.damontic.rng

import org.scalatest.FunSuite

class SimpleRNGFunSuite extends FunSuite {

  test("SimpleRNG(42).nextInt = 16159453") {
    val (result, nextRNG) = co.damontic.rng.SimpleRNG(42).nextInt
    assert(result === 16159453)
  }

  test("SimpleRNG.randomPair(SimpleRNG(42)) = ((16159453, -1281479697), rng)") {
    val ((result, result2), rng) = SimpleRNG.randomPair(SimpleRNG(42))
    assert(result === 16159453)
    assert(result2 === -1281479697)
    assert(rng.isInstanceOf[co.damontic.rng.SimpleRNG])
  }

  test("SimpleRNG.nonNegativeInt >= 0") {
    val rng1 = SimpleRNG(Int.MaxValue)
    val (r1, rng1_2) = SimpleRNG.nonNegativeInt(rng1)
    assert(r1 >= 0)

    val rng2 = SimpleRNG(Int.MinValue)
    val (r2, rng2_2) = SimpleRNG.nonNegativeInt(rng2)
    assert(r2 >= 0)
  }

  test("SimpleRNG.double in [0; 1[ ") {
    val rng1 = SimpleRNG(Int.MaxValue)
    val (r1, rng1_2) = SimpleRNG.double(rng1)
    assert(r1 >= 0 && r1 < 1)

    val rng2 = SimpleRNG(Int.MinValue)
    val (r2, rng2_2) = SimpleRNG.double(rng2)
    assert(r2 >= 0 && r2 < 1)
  }

  test("SimpleRNG.ints(10) size of list must be 10 ") {
    val rng = SimpleRNG(Int.MaxValue)
    val (lista, rng1) = SimpleRNG.ints(10)(rng)
    assert(lista.size == 10)
  }

}