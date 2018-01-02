package fibonacci

object fibonacci {

  val WarmupRounds = 1000
  val ActualRounds = 1000

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
    var result = List.empty[Long]
    var i = 0
    while (i < ActualRounds) {
      val t = System.nanoTime()
      run()
      val e = System.nanoTime() - t
      i = i + 1
      result = e :: result
    }
    median(result)
  }

  def median(xs: List[Long]): Long = {
    if (xs.isEmpty) throw new IllegalArgumentException("Empty list.")
    if (xs.length == 1) return xs.head

    val l = xs.sorted
    val n = xs.length
    if (n % 2 == 0) {
      val index = n / 2
      l(index)
    } else {
      val index = n / 2
      (l(index) + l(index + 1)) / 2
    }
  }

  def main(args: Array[String]): Unit = {
    warmpup()
    println(sample() / (1000 * 1000))
  }

}

