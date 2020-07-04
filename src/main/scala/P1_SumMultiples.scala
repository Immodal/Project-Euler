object P1 {
  /**
   * https://en.wikipedia.org/wiki/Arithmetic_progression
   * https://www.mathblog.dk/project-euler-problem-1/
   */
  def sumMultiples(mult: Long, lim: Long): Long = {
    // Calculate number of terms in the series
    val n = Math.floor(lim/mult).toLong
    // Get the arithmetic series from 1 to n,
    // then multiplying by mult will produce the series for range [mult, ..., n*mult)
    mult * (n*(n+1)/2)
  }

  /**
    * Because we are adding sum of multiples of x to sum of multiples of y,
    * their common multiples will be repeated. 
    * One occurrence of these common multiples needs to be subtracted to get the right output.
    */
  def sumUnionOfMultiples(x: Long, y: Long, lim:Long): Long = {
    sumMultiples(x, lim) + sumMultiples(y, lim) - sumMultiples(x * y, lim)
  }

  def main(args: Array[String]) {
    // Assuming input is "3 5 1000"
    var x = args(0).toInt
    var y = args(1).toInt
    var lim = args(2).toInt - 1
    
    println(sumUnionOfMultiples(x, y, lim))
  }
}