import scala_book._

class MyTreeTest extends UnitSpec {
  "size" should "work" in {
    val tree = Branch(Leaf(1), Leaf(2))
    assert(tree.size == 3)
  }

  "depth" should "work" in {
    val tree = Branch(Leaf(1), Leaf(2))
    assert(tree.depth == 2)
  }

  "maximum" should "work" in {
    val tree = Branch(Leaf(1), Leaf(3))
    assert(MyTree.maximum(tree) == 3)
  }
}
