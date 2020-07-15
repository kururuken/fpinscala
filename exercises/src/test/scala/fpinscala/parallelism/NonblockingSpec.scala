package fpinscala.parallelism

import java.util.concurrent._
import org.scalatest.{run => _, _}
import Nonblocking.Par._

class NonblockingSpec extends FlatSpec with Matchers {
    val es: ExecutorService = Executors.newCachedThreadPool()

    "Nonblocking" should "calculate" in {
        val p1 = unit(2)
        val p2 = unit(3)

        val p3 = map2(p1, p2)(_ + _)
        Nonblocking.Par.run(es)(p3) should equal (5)

        // this means ALL the computation will be delayed to the time when we call run
        // ALL not only means the evaluation, but also the construction of the computation flow
        // because fork has a by-name argument
        val p4 = fork(p3)
        Nonblocking.Par.run(es)(p4) should equal (5) 
    }

    // it should "make sequence" in {
    //     val l = List.fill(3)(unit(2))

    //     Nonblocking.run(es)(sequence(l)).get should equal (List.fill(3)(2))
    // }

    // it should "filter" in {
    //     val l = List(1, 2, 3)

    //     Nonblocking.run(es)(NonblockingFilter(l)(_ % 2 == 1)).get should equal (List(1, 3))
    // }

    it should "choidce" in {
        val l = List(1, 2, 3).map(unit(_))
        val idx = unit(2)

        Nonblocking.Par.run(es)(choiceN(idx)(l)) should equal (3)
        Nonblocking.Par.run(es)(choiceNViaFlatMap(idx)(l)) should equal (3)
        Nonblocking.Par.run(es)(choiceViaChoiceN(unit(true))(unit(1), unit(2))) should equal (1)
        Nonblocking.Par.run(es)(choiceViaFlatMap(unit(true))(unit(1), unit(2))) should equal (1)
    }
}