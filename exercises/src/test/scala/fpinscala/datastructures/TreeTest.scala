package fpinscala.datastructures

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    "tree" should "count its size" in {
        Tree.size(t) should equal (7)
        Tree.size2(t) should equal (7)
    }

    it should "has the miximum" in {
        Tree.maximum(t) should equal (4)
        Tree.maximum2(t) should equal (4)
    }

    it should "count the depth" in {
        Tree.depth(t) should equal (3)
    }

    it should "support map" in {
        Tree.map(t)(_ + 1) should equal (Branch(Branch(Leaf(2), Leaf(3)), Branch(Leaf(4), Leaf(5))))
    }

}