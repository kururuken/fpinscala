package fpinscala.errorhandling

import org.scalatest._
import scala.{Option => _, Some => _, Either => _, _}

class OptionSpec extends FlatSpec with Matchers {
    val sm = Some(1)
    val non: Option[Int] = None

    "option" should "map" in {
        sm map (_ + 1) should equal (Some(2))
        non map (_ + 1) should equal (None)
    }

    it should "get or else" in {
        sm getOrElse 2 should equal (1)
        non getOrElse 2 should equal (2)
    }

    it should "flat map" in {
        sm flatMap (x => Some(x + 1)) should equal (Some(2))
        sm flatMap (_ => None) should equal (None)
        non flatMap (x => Some(x+ 1)) should equal (None)
    }

    it should "filter" in {
        sm filter (_ == 1) should equal (Some(1))
        sm filter (_ == 2) should equal (None)
        non filter (_ == 2) should equal (None)
    }

    "variance" should "caculate with option" in {
        val s1 = Seq(0.0, 3.0, 6.0)
        val s2 = Seq()

        Option.variance(s1) should equal (Some(6.0))
        Option.variance(s2) should equal (None)
    }

    "sequence" should "collapse" in {
        val l1 = List(Some(1), Some(2))
        val l2 = List(Some(1), None)

        Option.sequence(l1) should equal (Some(List(1, 2)))
        Option.sequence2(l1) should equal (Some(List(1, 2)))
        Option.sequence(l2) should equal (None)
        Option.sequence2(l2) should equal (None)
    }


}