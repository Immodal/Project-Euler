object P2 {
  /**
    * Generate Fibonacci Sequence up to lim
    */
  def fib(lim: Long) : List[Long] = {
    def _fib(s: List[Long]) : List[Long] = {
      val x = s(0) + s(1)
      if (x>lim) s
      else _fib(x :: s)
    }

    _fib(List(1,1))
  }

  def main(args: Array[String]) {
    // Assuming input is "4000000"
    // Generate Fibonacci sequence and then fold it, only summing even values
    println(fib(args(0).toLong).fold(0L)((a, b) => if(b%2==0L) a+b else a))
  }
}