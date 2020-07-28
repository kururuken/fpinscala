package fpinscala.monoids

import org.scalatest._
import Monoid._
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds
import fpinscala.parallelism.Nonblocking
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions

class MonoidSpec extends FlatSpec with Matchers {
    "Monoid" should "do flat mao in parallel" in {
        val l = IndexedSeq("a", "b", "c", "d", "e")

        foldMapV(l, stringMonoid)(x => x) should equal ("abcde")
        foldMapV(IndexedSeq(""), stringMonoid)(x => x) should equal ("")
    }

    it should "using par implemented in previous chapters" in {
        val l = IndexedSeq("a", "b", "c", "d", "e")
        val es = Executors.newCachedThreadPool()

        parFoldMap(l, stringMonoid)(x => x).run(es) should equal ("abcde")
    }

    it should "be used to implement a word count" in {
        List("", "a", " s ", "s ", " s", "a s", " a s", "a s ", "a  s", " a s ").map(count) should equal (List(0, 1, 1, 1, 1, 2, 2, 2, 2, 2))
    }

    it should "count the occurence in a sequence" in {
        bag(IndexedSeq("a", "a", "b", "b", "b", "c")) should equal (Map(("a", 2), ("b", 3), ("c", 1)))
    }
}