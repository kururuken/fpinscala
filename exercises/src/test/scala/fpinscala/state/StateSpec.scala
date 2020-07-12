package fpinscala.state

import org.scalatest._
import fpinscala.state.RNG.Simple
import RNG._

class StateSpec extends FlatSpec with Matchers {
    val rng = Simple(1145141919810l)

    "Rand" should "know how to use it" in {
        int(rng)._1 should equal (int(rng)._1)
        // Usage is still the same if we want to generate the random number several times
        // val (i1,rng2) = rng.nextInt
        // val (i2,rng3) = rng2.nextInt
        // to
        // val (i1,rng2) = RNG.int(rng)
        // val (i1,rng3) = RNG.int(rng2)
    }

    it should "yield non negative int" in {
        list(100)(nonNegativeInt)(rng)._1 forall (_ >= 0) should equal (true)
    }

    it should "expand to list" in {
        val l = List.fill(10)(int)

        sequence3(l)(rng) should equal (sequence2(l)(rng))
    }
}