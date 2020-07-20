package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.state.State.Rand
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}
import org.scalatest._

class GenSpec extends FlatSpec with Matchers {
    val gen = unit(5)

    "Gen" should "unit" in {
        gen.run should equal (5)
    }

    it should "choose from a range" in {
        val g = choose(0, 10).run

        g should be <10
        g should be >= 0
    }

    it should "generate a list of length N" in {
        val g = listOfN(3, gen).run

        g should have length 3
        g.forall(_ == 5) should be (true)
    }

    "SGen" should "generate a list of given size n" in {
        val sgen = listOf(gen)
        val g = sgen.run(3)

        g should have length 3
        g.forall(_ == 5) should be (true)
    }

    "Prop" should "test a simple case" in {
        val prop = forAll(listOf(Gen.choose(0, 20))) { xs => xs.sorted match {
            case List() => true
            case List(_) => true
            case sorted @ _ => sorted.sliding(2).forall { case List(x, y) => x <= y}
        }}

        prop.run(100, 100, RNG.Simple(System.currentTimeMillis)) should be (Passed)
    }
}