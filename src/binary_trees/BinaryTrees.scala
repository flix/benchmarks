package binary_trees

object BinaryTrees {

  // binary_trees, from the Computer Language Benchmarks Game.
  // http://benchmarksgame.alioth.debian.org/u64q/binarytrees-description.html#binarytrees

  // This implementation is loosely based on:
  // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=binarytrees&lang=fsharpcore&id=8

  val WarmupRounds = 1000
  val ActualRounds = 1000

  val MinHeight: Int = 4
  val MaxHeight: Int = 21

  sealed trait Tree

  object Tree {

    object Singleton extends Tree

    case class Node(ls: Tree, rs: Tree) extends Tree

  }

  def makeTree(height: Int): Tree =
    if (height > 0)
      Tree.Node(makeTree(height - 1), makeTree(height - 1))
    else Tree.Singleton

  def computeChecksum(tree: Tree): Int = tree match {
    case Tree.Singleton => 1
    case Tree.Node(ls, rs) => computeChecksum(ls) + computeChecksum(rs) + 1
  }

  def stretchTreeChecksum(): Int = computeChecksum(makeTree(MaxHeight + 1))

  def calculateHeights(h: Int): List[Int] = if (h > MaxHeight) Nil else h :: calculateHeights(h + 2)

  def treeFrequency(height: Int): Int = 1 << (MaxHeight - height + MinHeight)

  def calculateChecksumOfTrees(height: Int, number: Int, acc: Int): Int =
    if (number > 0)
      calculateChecksumOfTrees(height, number - 1, computeChecksum(makeTree(height)) + acc)
    else
      acc

  def run(): Unit = {
    val temp_Tree_Checksum = stretchTreeChecksum()
    val long_Lived_Tree = makeTree(MaxHeight)

    val height_list = calculateHeights(MinHeight)
    val result = height_list.map(height => calculateChecksumOfTrees(height, treeFrequency(height), 0))
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
