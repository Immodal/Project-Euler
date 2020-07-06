object P6 {
  def main(args: Array[String]) {
    // Assuming input is "100"
    val numbers = (1L to args(0).toLong).toList
    println((Math.pow(numbers.sum, 2) - numbers.map(Math.pow(_, 2)).sum).toLong)
  }
}