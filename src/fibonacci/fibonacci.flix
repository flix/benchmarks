namespace Fibonacci {

  def fibonacci(): Int64 = fib(35i64)

  def fib(n: Int64): Int64 =
    if (n == 0i64) 0i64
    else if (n == 1i64) 1i64
    else fib(n - 1i64) + fib(n - 2i64)
}
