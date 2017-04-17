object fibonacci {

  val N: Long = 35

  def fib(n: Long): Long =
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)

  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    val result = fib(N)
    val end = System.nanoTime()
    val elapsed = (end - start) / 1000000

    println(s"Time: $elapsed ms")
    println(s"Result: $result")
  }

}

