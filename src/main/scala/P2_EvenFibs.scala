object P2 extends App {
  def fib(lim: Int) : List[Int] = {
    def _fib(s: List[Int]) : List[Int] = {
      val x = s(0) + s(1)
      if (x>lim) s
      else _fib(x :: s)
    }

    _fib(List(1,1))
  }

  // Generate Fibonacci sequence and then fold it, only summing even values
  println(fib(4000000).fold(0)((a, b) => if(b%2==0) a+b else a))
}