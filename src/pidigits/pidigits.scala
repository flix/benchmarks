package pidigits

object pidigits {

  val MaxRounds = 50
  val WarmupRounds = 100

  // pidigits, from the Computer Language Benchmarks Game.
  // http://benchmarksgame.alioth.debian.org/u64q/pidigits-description.html#pidigits

  // This implementation is a loose translation of the Flix program, which was loosely based on:
  // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=pidigits&lang=yarv&id=3

  val N: BigInt = 1000

  def compTpl1(a2: BigInt, n1: BigInt, d1: BigInt, t1: BigInt, u: BigInt): (BigInt, BigInt) = {
    val tmp = (n1 * 3) + a2
    if (a2 >= n1) (tmp / d1, (tmp % d1) + n1) else (t1, u)
  }

  def compTpl2(a2: BigInt, n1: BigInt, d1: BigInt, u1: BigInt, i: BigInt, t2: BigInt): (BigInt, BigInt, BigInt) =
    if ((a2 >= n1) && (d1 > u1)) (i - 1, (a2 - (d1 * t2)) * 10, n1 * 10) else (i, a2, n1)

  @scala.annotation.tailrec
  def piHelper(i: BigInt, k: BigInt, l: BigInt, n: BigInt, a: BigInt, d: BigInt, t: BigInt, u: BigInt): BigInt =
    if (i == 0) t
    else {
      val k1 = k + 1
      val t1 = n << 1
      val n1 = n * k1
      val a1 = a + t1
      val l1 = l + 2
      val a2 = a1 * l1
      val d1 = d * l1
      val (t2, u1) = compTpl1(a2, n1, d1, t1, u)
      val (i1, a3, n2) = compTpl2(a2, n1, d1, u1, i, t2)
      piHelper(i1, k1, l1, n2, a3, d1, t2, u1)
    }

  def pi(i: BigInt): BigInt = piHelper(i, 0, 1, 1, 0, 1, 0, 0)

  def run(): BigInt = pi(N)

  def warmpup(): Unit = {
    for (_ <- 0 until WarmupRounds) {
      run()
    }
  }

  def sample(): Long = {
    var elapsed = 0L
    for (_ <- 0 until MaxRounds) {
      val start = System.nanoTime()
      run()
      val end = System.nanoTime()
      elapsed = elapsed + (end - start)
    }
    elapsed / MaxRounds
  }

  def main(args: Array[String]): Unit = {
    warmpup()
    println(sample() / (1000 * 1000))
  }

}

