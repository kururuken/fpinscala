package fpinscala.errorhandling

import org.scalatest._
import scala.{Option => _, Either => _, Left => _, Right => _, _}

class EitherSpec extends FlatSpec with Matchers {
    val r: Either[Int, Int] = Right(1)
    val l: Either[Int, Int] = Left(1)

    "Either" should "map correctly" in {
        r.map(_ + 1) should equal (Right(2))
        l.map(_ + 1) should equal (Left(1))
    }

    it should "do map2" in {
        r.map2(Right(2))(_ + _) should equal (Right(3))
        r.map2(l)(_ + _) should equal (l) // map2 should return the left if one of the two members is left
        l.map2(r)(_ + _) should equal (l)
        l.map2((Left(2)))(_ + _) should equal (l) // both of them are left, should return the first one
        (Left(2): Either[Int, Int]).map2((l))(_ + _) should equal (Left(2))
    }
}