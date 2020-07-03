object P1 extends App {
  def sumMultiples(x: Int, lim: Int) = {
    (0 until lim by x).foldLeft(0)(_ + _)
  }

  /**
   * First sum multiples of 3 and 5.
   * Because 15 is a multiple of both 3 and 5, its multiple will appear twice.
   * (Once in multiples of 3 and once in multiples of 5)
   * So we subtract one appearance of multiples of 15.
   */
  println(sumMultiples(3, 1000) + sumMultiples(5, 1000) - sumMultiples(15, 1000))
}