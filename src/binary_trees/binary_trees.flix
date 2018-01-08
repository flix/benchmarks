namespace BinaryTrees {

    // binary_trees, from the Computer Language Benchmarks Game.
    // http://benchmarksgame.alioth.debian.org/u64q/binarytrees-description.html#binarytrees

    // This implementation is loosely based on:
    // http://benchmarksgame.alioth.debian.org/u64q/program.php?test=binarytrees&lang=fsharpcore&id=8

    enum Tree {
        case Singleton,
        case Node(Tree, Tree)
    }

    def minHeight(): Int = 4
    def maxHeight(): Int = 16

    def makeTree(height: Int): Tree =
        if (height > 0) 
            Node(makeTree(height - 1), makeTree(height - 1))
        else 
            Singleton

    def computeChecksum(tree: Tree): Int = match tree with {
        case Node(ls, rs) => computeChecksum(ls) + computeChecksum(rs) + 1
        case Singleton => 1
    }

    def stretchTreeChecksum(): Int = computeChecksum(makeTree(maxHeight() + 1))

    def calculateHeights(h: Int): List[Int] = if (h > maxHeight()) Nil else h :: calculateHeights(h + 2)

    def treeFrequency(height: Int): Int = 1 <<< (maxHeight() - height + minHeight())

    def calculateChecksumOfTrees(height: Int, number: Int, acc: Int): Int =
        if (number > 0)
            calculateChecksumOfTrees(height, number - 1, computeChecksum(makeTree(height)) + acc)
        else
            acc

    @benchmark
    def binaryTrees(): List[Int] =
        let temp_Tree_Checksum = stretchTreeChecksum();
        let long_Lived_Tree = makeTree(maxHeight());
        let height_list = calculateHeights(minHeight());
            List.map(height -> calculateChecksumOfTrees(height, treeFrequency(height), 0), height_list)
}
