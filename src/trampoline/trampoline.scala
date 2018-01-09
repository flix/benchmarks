package trampoline

import scala.annotation.tailrec

object trampoline {

  val WarmupRounds = 1000
  val ActualRounds = 1000

  val N = 12345678

  sealed trait Trampoline[+A]

  case class Done[A](a: A) extends Trampoline[A]

  case class More[A](a: () => Trampoline[A]) extends Trampoline[A]

  def eval[A](t: Trampoline[A]): A = t match {
    case Done(x) => x
    case More(f) => eval(f)
  }

  @tailrec
  def eval[A](f: () => Trampoline[A]): A = f() match {
    case Done(x) => x
    case More(g) => eval(g)
  }

  def even(n: Int): Trampoline[Boolean] = {
    if (n == 0) Done(true)
    else More(() => odd(n - 1))
  }

  def odd(n: Int): Trampoline[Boolean] = {
    if (n == 0) Done(false)
    else More(() => even(n - 1))
  }

  def run(): Boolean = {
    eval(even(N))
  }

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
