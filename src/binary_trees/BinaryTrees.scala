object BinaryTrees {
  // binary_trees, from the Computer Language Benchmarks Game.
  // http://benchmarksgame.alioth.debian.org/u64q/binarytrees-description.html#binarytrees

  // This implementation is loosely based on:
  // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=binarytrees&lang=fsharpcore&id=8

  sealed trait Tree
  case class Node(ls: Tree, rs: Tree) extends Tree
  object Singleton extends Tree

  val max_Height: Int = 21

  val min_Height: Int = 4

  def make_Tree(height: Int): Tree =
    if(height > 0) Node(make_Tree(height - 1), make_Tree(height - 1))
    else Singleton

  def compute_Checksum(tree: Tree): Int = tree match {
    case Node(ls, rs) => compute_Checksum(ls) + compute_Checksum(rs) + 1
    case Singleton => 1
  }

  def stretch_Tree_Checksum(): Int = compute_Checksum(make_Tree(max_Height + 1))

  def calculate_Heights(h: Int): List[Int] = if(h > max_Height) Nil else h :: calculate_Heights(h + 2)

  def tree_Frequency(height: Int): Int = 1 << (max_Height - height + min_Height)

  def calculate_Checksum_Of_Trees(height: Int, number: Int, acc: Int): Int =
    if(number > 0) calculate_Checksum_Of_Trees(height, number - 1, compute_Checksum(make_Tree(height)) + acc)
    else acc

  def main(args: Array[String]): Unit = {
    val now = System.currentTimeMillis()

    val temp_Tree_Checksum = stretch_Tree_Checksum()
    val long_Lived_Tree = make_Tree(max_Height)

    val height_list = calculate_Heights(min_Height)
    val result = height_list.map(height => calculate_Checksum_Of_Trees(height, tree_Frequency(height), 0))
    val timeElapsed = System.currentTimeMillis - now

    println(s"Time Elapsed in Millis: $timeElapsed")
    println(result)
  }

}
