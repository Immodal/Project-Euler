object P7 {
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
    * Returns a Vector of prime numbers if length tar.
    * Checks if every odd number x is a prime number, if it is, add the ps.
    * It returns a Vector instead of the number as in optimization for HackerRank.
    * LazyList prime number generator from P3 caused memory overflow.
    */
  def primes(tar: Long, ps: Vector[Long] = Vector(2), x: Long = 3): Vector[Long] = {
    if (tar==1) ps
    else if (ps.length==tar-1 && isPrime(x)) ps :+ x
    else if (isPrime(x)) primes(tar, ps :+ x, x+2)
    else primes(tar, ps, x+2)
  }

  def main(args: Array[String]) {
    // Assuming input is "10001"
    println(primes(args(0).toInt).last)
  }
}