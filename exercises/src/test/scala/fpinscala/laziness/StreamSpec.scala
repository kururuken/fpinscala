package fpinscala.laziness

import Stream._
import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {
    val s = Stream(1, 2, 3)

    // We can do a small test to see how things work
    // We change the functions so they have logs
    //
    // def foldRight[B](z: => B)(f: (A, => B) => B): B = 
    //   this match {
    //     case Cons(h,t) => f({println("1"); h()}, {println("2"); t().foldRight(z)(f)}) 
    //     case _ => z
    //   }
    // 
    // def toList: List[A] = 
    //     foldRight[List[A]](Nil)((h, t) => {println("3"); h :: t}) 
    //
    // And the following expression 
    // val tmp = Stream(1,2,3).map(_ + 10).filter(_ % 2 == 0)
    // will actually print 1, 1, 2, 2, 1, 1
    // 
    // And the following 
    // val tmp = Stream(1,2,3).map(_ + 10).filter(_ % 2 == 0).toList
    // will print 1, 1, 2, 2, 1, 1, 3, 2, 2, 2, 1, 1, 2, 2


    "Stream" should "convert to list" in {
        s.toList should equal (List(1, 2, 3))
    }

    it should "for all" in {
        s.forAll(_ >= 1) should equal (true)
    }

    it should "take and drop" in {
        s.take(2).toList should equal (List(1, 2))
        s.take2(2).toList should equal (List(1, 2))
        s.take(4).toList should equal (List(1, 2, 3))
        s.take2(4).toList should equal (List(1, 2, 3))
        s.drop(2).toList should equal (List(3))
        s.takeWhile(_ <= 2).toList should equal (List(1, 2)) // Stream cannot be compared directly
        s.takeWhile2(_ <= 2).toList should equal (List(1, 2))
    }

    it should "head" in {
        s.headOption should equal (Some(1))
        Stream().headOption should equal (None)
    }

    it should "map" in {
        s.map(_ + 1).toList should equal (List(2, 3, 4))
        s.map2(_ + 1).toList should equal (List(2, 3, 4))
    }

    it should "filter" in {
        s.filter(_ != 2).toList should equal (List(1, 3))
    }

    it should "append" in {
        s.append(s).toList should equal (List(1, 2, 3, 1, 2, 3))
    }

    it should "flat map" in {
        s.flatMap(x => Stream(x, x)).toList should equal (List(1, 1, 2, 2, 3, 3))
    }

    it should "zip with another stream" in {
        s.zipWith(s)(_ + _).toList should equal (List(2, 4, 6))
        s.zipAll(Stream(1)).toList should equal (List((Some(1), Some(1)), (Some(2), None), (Some(3), None)))
    }

    it should "start with another sequence" in {
        s.startsWith(Stream(1, 2)) should equal (true)
        s.startsWith(Stream(3)) should equal (false)
        s.startsWith(Stream()) should equal (true)
    }

    it should "give all its tails" in {
        s.tails.map(_.toList).toList should equal (Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).map(_.toList).toList)
    }

    "Infinite stream" should "take constant" in {
        Stream.constant(2).take(3).toList should equal (List(2, 2, 2))
        Stream.constant2(2).take(3).toList should equal (List(2, 2, 2))
    }

    it should "start from n" in {
        Stream.from(1).take(3).toList should equal (s.toList)
        Stream.from2(1).take(3).toList should equal (s.toList)
    }

    it should "calculate fibs" in {
        Stream.fibs.take(3).toList should equal (List(0, 1, 1))
        Stream.fibs2.take(3).toList should equal (List(0, 1, 1))
    }
}