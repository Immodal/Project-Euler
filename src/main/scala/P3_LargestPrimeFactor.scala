object P3 {
  /**
    * Modified version of LazyList.from() to return infinite LazyList[Long] from start with step
    * https://github.com/scala/scala/blob/v2.12.8/src/library/scala/collection/immutable/Stream.scala#L1
    */
  def from(start: Long, step: Long): LazyList[Long] = LazyList.cons(start, from(start+step, step))

  /**
    * Sieve of Eratosthenes
    * https://gist.github.com/motylwg/5868521#file-primesstreamexample-scala-L6
    */
  def sieve(s: LazyList[Long]): LazyList[Long] = s.head #:: sieve(s.tail filter(_ % s.head != 0))

  /**
    * A LazyList of primes generated using the Sieve of Eratosthenes
    */
  val primes: LazyList[Long] = sieve(2 #:: from(3, 2))

  /**
    * Returns List of all prime factors of n, in descending order
    */
  def getPrimeFactors(n: Long): List[Long] = {
    def pf(x:Long, i: Int, s: List[Long]): List[Long] = {
      if (x == 1) s
      else if (x % primes(i) == 0) pf(x/primes(i), i, primes(i) :: s)
      else if (primes(i+1) <= Math.sqrt(x)) pf(x, i+1, s)
      else x :: s
    }

    pf(n, 0, List())
  }

  def main(args: Array[String]) {
    // Assuming input is "600851475143"
    println(getPrimeFactors(args(0).toLong)(0))
  }
}