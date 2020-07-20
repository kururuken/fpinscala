package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.state.State.Rand
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (m, t, r) =>
    run(m, t, r) match {
      case Passed => p.run(m, t, r)
      case f @ Falsified(failure, success) => f
    }
  }

  def ||(p: Prop): Prop = Prop { (m, t, r) =>
    run(m, t, r) match {
      case Passed => Passed
      case f @ Falsified(failure, success) => p.run(m, t, r)
    }
  }

  def tag(str: String): Prop =  Prop { (m, t, r) =>
    run(m, t, r) match {
      case Passed => Passed
      case Falsified(failure, success) => Falsified(str + "\n" + failure, success)
    }
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, success: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

    /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  // I don't understand what MaxSize is in Prop
  // Maybe it just want to control the test case size 
  // Consider the following scenerio
  // We want to test some functions w.r.t a list and we have a function
  // f: List[A] => Boolean
  // In the old interface Prop(run: (TestCases, RNG) => Result)
  // We have no control over the size of the test case we automatically generate
  // That is to say, we might generate 100 cases, among which the first one might yield
  // a list of length 345, second one 72, etc
  // However, we want the test sequence to be of increasing order, so we will fail at the minimum test case possible
  // i.e. if the test will fail at the length of 3
  // we want our test sequence(length) to be 0, 1, 2, 3 which is most efficient
  // rather than 345, 72, 763 ...... 3 (maybe after 80 tries)
  // Also we want to control the total size of the possible test case generated, say 100, so each test cases will not generate
  // a list of which the size exceeds 100 for the sake of efficiency
  // So the whole idea becomes this:
  // we define two demensions of a test: max test size and number of test cases
  // max test size controls the size of one generated test case (i.e. the length of a generated list)
  // and number of test cases controls the total number of test cases generated.
  // We want the test case sequence to be of inceasing order of test size, 0, 1, 2, 3...
  // and for each size we generate certain number of cases
  // To do this, we introduce a new generator type SGen,
  // it takes an int and outputs a Gen that will generate something for the given size

  // In fact I don't understand the whole testing system
  // Why is it useful? 
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def apply(f: (TestCases, RNG) => Result): Prop = 
    Prop((_, t, r) => f(t, r))
  
}

object Gen {
  implicit val rng: RNG = RNG.Simple(System.currentTimeMillis)

  def unit[A](a: => A): Gen[A] = 
    new Gen(State.unit(a))
  
  def choose(start: Int, stopExclusive: Int): Gen[Int] = 
    new Gen(Rand.interval(start, stopExclusive))

  def boolean: Gen[Boolean] = new Gen(Rand.nonNegativeLessThan(2)).map(x => if (x == 0) true else false)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = 
    new Gen(State.sequence(List.fill(n)(g.sample)))
  
  def listOf[A](g: Gen[A]): SGen[List[A]] = 
    SGen(n => listOfN(n, g))
  
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
    boolean.flatMap(x => if (x) g1 else g2)
  
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = 
    new Gen(Rand.double).flatMap(x => if (x < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1)

}

case class Gen[+A](sample: State.Rand[A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = 
    size.flatMap(Gen.listOfN(_, this))
  
  def unsized: SGen[A] = SGen(_ => this)

  def run(implicit rng: RNG): A = sample.run(rng)._1

}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int) = g(n)

  def map[B](f: A => B): SGen[B] = SGen(g(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => g(n).flatMap(f(_).g(n)))

  def run(n: Int): A = g(n).run

}

