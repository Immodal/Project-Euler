object P9 {
  /**
    * Given the following:
    *   a < b < c
    *   a + b + c = n .....................(1)
    *   a^2 + b^2 = c^2 ...................(2)
    * From (1) we get:
    *   c = n - a - b .....................(3)
    * Substitute (3) into (2):
    *   a^2 + b^2 = (n - a - b)^2 .........(4)
    * Expanding (4), we can express b in terms of a and n:
    *   b = ((n*n)-2*n*a)/(-2*(a-n)) ......(5)
    */

  def getC(n: Long, a: Long, b: Long): Long= n-a-b

  def getB(n: Long, a: Long): Long = ((n*n)-2*n*a)/(-2*(a-n))

  def findA(n: Long): List[Long] = {
    // because a<b<c, a will never be greater than n/3
    (1L to n/3).toList
      .filter(a => {
        val b = getB(n, a)
        val c = getC(n, a, b)
        a<b && b<c && a*a + b*b == c*c 
      })
  }

  def getABC(n: Long): Long = {
    val as = findA(n)
    if(as.length>0) {
      // Some ns have multiple triplets, 
      // but we only want the largest a for HackerRank.
      val a = as.last
      val b = getB(n, a)
      val c = getC(n, a, b)
      a * b * c
    } else -1
  }

  def main(args: Array[String]) {
    // Assumes input is 1000
    println(getABC(args(0).toLong))
  }
}