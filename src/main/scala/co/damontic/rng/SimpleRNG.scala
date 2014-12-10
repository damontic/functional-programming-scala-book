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

/* exercise 6.1
 * Write a function that uses RNG.nextInt to generate a random integer between 0 and
 * Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
 * Int.MinValue, which doesn’t have a non-negative counterpart.
 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInt, rng2) = rng.nextInt
    if(randomInt == Int.MinValue) (Int.MaxValue-1, rng)
    else (Math.abs(randomInt), rng2)
  }

/* exercise 6.2
 * Write a function to generate a Double between 0 and 1, not including 1 . Note: You can
 * use Int.MaxValue to obtain the maximum positive integer value, and you can use
 * x.toDouble to convert an x: Int to a Double.
 */
  def double(rng: RNG): (Double, RNG) = {
    val (randomInt, rng2) = SimpleRNG.nonNegativeInt(rng)
    ((randomInt/Int.MaxValue.toDouble), rng2)
  }
  
/* exercise 6.3
 * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
 * (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
 * already written.
 */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (doubleRandom, rng2) = SimpleRNG.double(rng)
    val (integerRandom, rng3) = rng2.nextInt
    ((integerRandom,doubleRandom), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((integerRandom, doubleRandom), rng2) = SimpleRNG.intDouble(rng)
    ((doubleRandom, integerRandom), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rng1) = SimpleRNG.double(rng)
    val (double2, rng2) = SimpleRNG.double(rng1)
    val (double3, rng3) = SimpleRNG.double(rng2)
    ((double1, double2, double3), rng3)
  }

  /*exercise 6.4
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    
    def loop(lista: List[Int], rng: RNG, counter: Int) : (List[Int], RNG) = {
      if(counter == count) (lista, rng)
      else {
        val (iterationRandom, iterationRNG) = rng.nextInt
        loop( iterationRandom :: lista , iterationRNG, counter + 1) 
      }
    }
    
    loop(Nil, rng, 0)
    
  }
  
  /*-------------------------------------------------*/
  type Rand[+A] = RNG => (A, RNG)
  
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  
  /*  excercise 6.5
   *   Use map to reimplement double in a more elegant way. See exercise 6.2.
   */
//  def double(rng: RNG): (Double, RNG) = {
//    
//  }

  
}