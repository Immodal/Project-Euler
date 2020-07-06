object P5 {
  /**
    * Modified version of LazyList.from() to return infinite LazyList[Long] from start with step
    * https://github.com/scala/scala/blob/v2.12.8/src/library/scala/collection/immutable/Stream.scala#L1
    */
  def from(start: Long, step: Long = 1): LazyList[Long] = LazyList.cons(start, from(start+step, step))

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

  /**
    * Returns the smallest value that can be divided by each number from 2 to x with no remainders
    * Mathblog's explanation helped in understanding how to reason about the problem
    * https://www.mathblog.dk/project-euler-problem-5/
    */
  def getSmallestMultiple(x: Long): Long = {
    // Get numbers up to x,
    if (x>1) from(2).takeWhile(_ <= x)
      // Get the prime factors for each number,
      .map(getPrimeFactors(_))
      // If pfs is a subset of acc, do nothing, otherwise add the difference,
      .fold(List())((acc, pfs) => acc ::: pfs.diff(acc))
      // Multiply all factors together.
      .reduce(_ * _)
    else x
  }

  def main(args: Array[String]) {
    // Assuming input is "20"
    println(getSmallestMultiple(args(0).toLong))
  }
}