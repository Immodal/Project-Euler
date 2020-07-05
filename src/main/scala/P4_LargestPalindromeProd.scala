object P4 {
  /**
    * Returns true if x is a palindrome
    * Alternative to string based comparison
    */
  def isPalindrome (x: Long): Boolean = {
    // Returns a reversed version "a"
    def reverse (a: Long, b: Long = 0): Long = {
      // Integer division by 10 removes the rightmost digit from "a"
      // Add that digit to b after moving all digits of b up one space 
      // (by multiplying by 10)
      if (a!=0) reverse(a/10, b * 10 + a % 10)
      else b
    }

    x == reverse(x)
  }

  /**
    * Returns largest palindrome that is 6 digits and is smaller than lim
    *             p * q = 11 * (9091 * a + 910 * b + 100 * c) <= 999**2
    * Robert Eisele does a great job explaining the math behind this algorithm in detail
    * https://www.xarg.org/puzzle/project-euler/problem-4/
    */
  def getLargestPalindrome(lim: Long, r: Long = 0, p: Long = 990, q: Long = 999): Long = {
    val t = p * q
    // Finished our search, so return r
    // a p or q that is <100 can't produce a 6 digit number so they are omitted
    if (p < 99) r
    // Found a larger palindrome that is within limits
    // Because p must be a multiple of 11, we start it at the largest which is 990 and can decrement it by 11
    // If t > r, the values of q that are less than the current can be omitted, so it is reset to 999
    else if (t > r && t < lim && isPalindrome(t)) getLargestPalindrome(lim, t, p-11, 999)
    // If t < r, the lower values of q can be omitted
    else if (t < r || q <= 99) getLargestPalindrome(lim, r, p-11, 999)
    else getLargestPalindrome(lim, r, p, q-1)
  }

  def main(args: Array[String]) {
    // Assuming input is "1000000"
    println(getLargestPalindrome(args(0).toLong))
  }
}