package fibonacci

object fibonacci {

  val WarmupRounds = 100
  val ActualRounds = 50

  val N: Long = 35

  def fib(n: Long): Long =
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)

  def run(): Long = fib(N)

  def warmpup(): Unit = {
    for (_ <- 0 until WarmupRounds) {
      run()
    }
  }

  def sample(): Long = {
    var elapsed = 0L
    for (_ <- 0 until ActualRounds) {
      val start = System.nanoTime()
      run()
      val end = System.nanoTime()
      elapsed = elapsed + (end - start)
    }
    elapsed / ActualRounds
  }

  def main(args: Array[String]): Unit = {
    warmpup()
    println(sample() / (1000 * 1000))
  }

}

