package fpinscala.datastructures

import org.scalatest._
import scala.{List => _, Nil => _, _}

class ListSpec extends FlatSpec with Matchers {

    val l = List(1, 2, 3)
    val l2 = List(1.0, 2.0, 3.0)

    "list" should "yield proper sum and product" in {
        List.sum(l) should equal (6)
        List.sum2(l) should equal (6)
        List.sum3(l) should equal (6)
        List.product(l2) should equal (6.0)
        List.product2(l2) should equal (6.0)
        List.product3(l2) should equal (6.0)
    }

    it should "compute length" in {
        List.length(l) should equal (3)
        List.length2(l) should equal (3)
    }

    it should "yield tail" in {
        List.tail(l) should equal (List(2, 3))
        List.tail(List(1)) should equal (Nil)
    }

    it should "set head" in {
        List.setHead(l, 4) should equal (List(4, 2, 3))
        List.setHead(Nil, 1) should equal (List(1))
    }

    it should "drop first n" in {
        List.drop(l, 2) should equal (List(3))
    }

    it should "not drop if n < 0" in {
        List.drop(l, -1) should equal (l)
    }

    it should "become nil if n > size" in {
        List.drop(l, 4) should equal (Nil)
        List.drop(Nil, 1) should equal (Nil)
        List.drop(Nil, -2) should equal (Nil)
    }

    it should "reverse" in {
        List.reverse(l) should equal (List(3, 2, 1))
    }

    it should "append and concat" in {
        List.append2(l, l) should equal (List(1, 2, 3, 1, 2, 3))
        List.concat(List(l, l)) should equal (List(1, 2, 3, 1, 2, 3))
        List.append2(l, Nil) should equal (l)
        List.append2(Nil, l) should equal (l)
        List.concat(List(l, Nil)) should equal (l)
    }

    it should "init values" in {
        List.init(l) should equal (List(1, 2))
        List.init2(l) should equal (List(1, 2))
    }

    it should "map 2 new values" in {
        List.map(l)(_ + 1) should equal (List(2, 3, 4))
        List.map2(l)(_ + 1) should equal (List(2, 3, 4))
    }

    it should "flat map" in {
        List.flatMap(l)(x => List(x, x)) should equal (List(1, 1, 2, 2, 3, 3))
    }

    it should "filter" in {
        List.filter(l)(_ >= 2) should equal (List(2, 3))
        List.filter2(l)(_ >= 2) should equal (List(2, 3))
    }

    it should "zip" in {
        List.zipWith(l, l)(_ + _) should equal (List(2, 4, 6))
    }
}