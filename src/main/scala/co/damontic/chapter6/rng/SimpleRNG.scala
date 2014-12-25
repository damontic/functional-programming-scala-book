package co.damontic.rng

trait RNG {
  def nextInt: (Int, RNG)
}

class SimpleRNG(val seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object SimpleRNG {

  def apply(seed: Long): SimpleRNG = new SimpleRNG(seed)

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (randomNumber1, rng2) = rng.nextInt
    val (randomNumber2, rng3) = rng2.nextInt
    ((randomNumber1, randomNumber2), rng3)
  }

  /**
   * exercise 6.1
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and
   * Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
   * Int.MinValue, which doesn’t have a non-negative counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInt, rng2) = rng.nextInt
    if (randomInt == Int.MinValue) (Int.MaxValue - 1, rng)
    else (Math.abs(randomInt), rng2)
  }

  /**
   * exercise 6.2
   * Write a function to generate a Double between 0 and 1, not including 1 . Note: You can
   * use Int.MaxValue to obtain the maximum positive integer value, and you can use
   * x.toDouble to convert an x: Int to a Double.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (randomInt, rng2) = SimpleRNG.nonNegativeInt(rng)
    ((randomInt / Int.MaxValue.toDouble), rng2)
  }

  def int(rng: RNG): (Int, RNG) = {
    rng.nextInt
  }

  /**
   * exercise 6.3
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
   * (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
   * already written.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (doubleRandom, rng2) = SimpleRNG.double(rng)
    val (integerRandom, rng3) = rng2.nextInt
    ((integerRandom, doubleRandom), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((integerRandom, doubleRandom), rng2) = SimpleRNG.intDouble(rng)
    ((doubleRandom, integerRandom), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng1) = SimpleRNG.double(rng)
    val (double2, rng2) = SimpleRNG.double(rng1)
    val (double3, rng3) = SimpleRNG.double(rng2)
    ((double1, double2, double3), rng3)
  }

  /**
   * exercise 6.4
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def loop(lista: List[Int], rng: RNG, counter: Int): (List[Int], RNG) = {
      if (counter == count) (lista, rng)
      else {
        val (iterationRandom, iterationRNG) = rng.nextInt
        loop(iterationRandom :: lista, iterationRNG, counter + 1)
      }
    }

    loop(Nil, rng, 0)

  }

  /*-------------------------------------------------*/

  /**
   * To make the type of actions convenient to talk about, and to simplify our thinking
   * about them, let’s make a type alias for the RNG state action data type
   */
  type Rand[+A] = RNG => (A, RNG)

  /**
   * We can now turn methods such as RNG ’s nextInt into values of this new type
   */
  // val int: Rand[Int] = _.nextInt

  /**
   * We want to write combinators that let us combine Rand actions while avoiding explicitly
   *  passing along the RNG state. We’ll end up with a kind of domain-specific language
   *  that does all of the passing for us.
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /**
   * transforms the output of a state action without modifying the state itself
   */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
   * As an example of how map is used, here’s nonNegativeEven , which reuses nonNegativeInt
   * to generate an Int that’s greater than or equal to zero and divisible by two
   */
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /**
   * excercise 6.5
   *   Use map to reimplement double in a more elegant way. See exercise 6.2.
   */
  def double2(rng: RNG): Rand[Double] = {
    val s: Rand[Int] = nonNegativeInt
    val f: Int => Double = {
      i => i / Int.MaxValue.toDouble
    }
    map(s)(f)
  }

  /**
   * excercise 6.6
   * Write the implementation of map2 based on the following signature. This function
   * takes two actions, ra and rb , and a function f for combining their results, and returns
   * a new action that combines them:
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (randomA, rng1) = ra(rng)
        val (randomB, rng2) = rb(rng1)
        (f(randomA, randomB), rng2)
      }
  }

  /**
   * We only have to write the map2 combinator once, and then we can use it to combine
   * arbitrary RNG state actions. For example, if we have an action that generates values of
   * type A and an action to generate values of type B, then we can combine them into one
   * action that generates pairs of both A and B :
   */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /**
   * excercise 6.7: hard
   * If you can combine two RNG transitions, you should be able to combine a whole
   * list of them. Implement sequence for combining a List of transitions into a single
   * transition. Use it to reimplement the ints function you wrote before. For the latter,
   * you can use the standard library function List.fill(n)(x) to make a list with x repeated n times.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng =>
      {

        def helper(actualList: List[A], counter: Int, actualRNG: RNG): (List[A], RNG) = {
          if (counter == fs.size) (actualList, actualRNG)
          else {
            val (random, nextRNG) = fs(counter)(actualRNG)
            helper(random :: actualList, counter + 1, nextRNG)
          }
        }
        helper(Nil, 0, rng)
      }
  }

  val randIntDoubleInt: Rand[List[AnyVal]] = sequence(List(int, double, int))

  /**
   *
   */
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {

    val fs: List[Rand[Int]] = List.fill(count)(
      rng_1 => {
        rng_1.nextInt
      })
    sequence(fs)(rng)

  }

  /****************************************************************************************/
  /**
   * It generates an integer between 0 (inclusive) and n (exclusive)
   * This version problem: skewed
   */
  def nonNegativeLessThan_v0(n: Int): Rand[Int] =
    map(nonNegativeInt) { _ % n }

  /**
   * Retry recursively if the Int we got is higher than the largest multiple of n that fits in a 32-bit Int.
   */
  //  def nonNegativeLessThan_v1(n: Int): Rand[Int] =
  //    map(nonNegativeInt) { i =>
  //      val mod = i % n
  //      if (i + (n - 1) - mod >= 0) mod else nonNegativeLessThan1(n)(???)
  //    }

  /**
   * We want to chain things together so that the RNG that’s returned by nonNegativeInt is passed
   * along to the recursive call to nonNegativeLessThan
   */
  def nonNegativeLessThan_v2(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan_v2(n)(rng)
  }

  /**
   * excercise 6.8
   * Implement flatMap , and then use it to implement nonNegativeLessThan.
   * flatMap allows us to generate a random A with Rand[A] , and then take that A and
   * choose a Rand[B] based on its value.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      {
        val (aVal, rng1) = f(rng)
        g(aVal)(rng1)
      }
  }

  /**
   * In nonNegativeLessThan , we use flatMap to choose
   * whether to retry or not, based on the value generated by nonNegativeInt.
   */
  def nonNegativeLessThan_v3(n: Int): Rand[Int] = {
    val f: Rand[Int] = rng => nonNegativeInt(rng)
        
    val g: Int => Rand[Int] = {
      i =>
        {
          rng =>
            {
              val mod = i % n
              if (i + (n - 1) - mod >= 0) (mod, rng)
              else nonNegativeInt(rng)
            }
        }
    }
    
    flatMap(f)(g)
  }

}