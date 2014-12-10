/**
 * Created by david on 1/11/14.
 */
object Fibonacci {
  def fib( n : Int )  = {
    if(n == 0) 0
    else if(n == 1) 1
    else{
      @annotation.tailrec
      def loop(actualValue: Int, fib_n_1 : Int, fib_n_2 : Int ): Int = {
        if(actualValue == n)  fib_n_1
        else loop(actualValue + 1 , fib_n_1 + fib_n_2 , fib_n_1)
      }
      loop(2, 1, 1)
    }
  }
}
