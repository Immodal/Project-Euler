object P10 {
  /**
    * Returns true if x is a prime
    */
  def isPrime(x: Long): Boolean = {
    if (x<=1 || x%2==0) false
    else if (x==2) true
    else {
      // Only checks odd numbers
      def _isPrime(y: Long): Boolean = {
        if(y * y > x) true
        else if(x % y == 0) false
        else _isPrime(y+2)
      } 
      
      _isPrime(3)
    }
  }

  /**
    * Returns a Vector of prime numbers up to and including max.
    * Checks if every odd number x is a prime number, if it is, add the ps.
    * Variation on P7 implementation
    */
  def primes(max: Long, ps: Vector[Long] = Vector(2), x: Long = 3): Vector[Long] = {
    if (x>max) ps
    else if (isPrime(x)) primes(max, ps :+ x, x+2)
    else primes(max, ps, x+2)
  }

  /**
    * Producing a master list, filtering for each subtask and then summing is too slow for HackerRank.
    * This function combines the filtering and summing steps given ps is arranged in ascending order.
    */
  def sumWhileLessThan(max: Long, ps: Vector[Long], i: Int = 0, acc: Long = 0): Long = {
    if(i>=ps.length || ps(i)>max) acc
    else sumWhileLessThan(max, ps, i+1, acc+ps(i))
  }

  def main(args: Array[String]) {
    // Assuming input is "2000000"
    println(primes(args(0).toLong).sum)
  }
}