package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt: Rand[Int] = 
    map(int)(x => if (x < 0) -(x + 1) else x)

  def double: Rand[Double] = 
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  def list[A](count: Int)(s: Rand[A]): Rand[List[A]] = 
    rng => {
      def go(count: Int, xs: List[A]): Rand[List[A]] = rng => {
        if (count == 0)
          (xs, rng)
        else {
          val (x, next_rng) = s(rng)
          go(count - 1, x :: xs)(next_rng)
        }
      }
      go(count, List())(rng)
    }

  def ints(count: Int): Rand[List[Int]] = 
    list(count)(int)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    rng => {
      fs.foldRight((List[A](), rng)) { case (rand, (xs, r)) => {
        val (x, rng1) = rand(r)
        (x :: xs, rng1)
      }}
    } // this is correct, don't know why it shows error

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))
  
  // actually sequence is the reverse of sequence 2 
  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] = 
    map(sequence(fs))(_.reverse)
  
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    flatMap(s)(x => unit(f(x)))
  
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

// We could write the general definition as 
// type State[S, +A] = S => (A, S)
// but the case class definition hides the underlying run function
// in the above example it would be the rng
// for example, in the map signature, we have
// def map[A,B](s: Rand[A])(f: A => B): Rand[B]
// but in the general case we have
// def map[B](f: A => B): State[S, B]
// so the run function (s: Rand[A]) is hidden by the
// class member run: S => (A, S)

// usage: 
// val f(S): (A, S) = ...
// val s = State(f)
//
// val oldState: S 
// val (result1, newState1) = s.run(oldState)
// val (result2, newState2) = s.run(newState1)

// We can see that the pre-defined function f is still in charge of 
// 1. computing the output according to the input state
// 2. giving the new state according to the old input state
// so the case class State just wraps some common functions we will use
// what it does is just modifying the trasition and computation function run
// by combining other functions 

// Note original one is 
// case class State[S,+A](run: S => (A, S))
// In order to make things easier for chapter 8 where we will make heavy use of 
// Rand[A] aka State[RNG, A]
// and some methods implemented above
// The reason why we make this change is that we cannot inherit a case class
// There are several alternatives like trait or abstract class (they are almost the same)
// However we cannot do that because our method flatMap (and also unit) requires us 
// to return a class instance. But a trait or abstract class cannot be instantiate.

// ABOVE HAS SOME PROBLEMS
// We cannot make Rand inherits State because it will has some problem
// for example we want to implement
// def nonNegativeInt: Rand[Int] = 
//    int.map(x => if (x < 0) -(x + 1) else x)
// We make use of the State class's method map
// The problem is that map is actually returning State[RNG, Int]
// However our function signature requires to return a Rand[Int]
// Here comes the problem: 
// State is the superclass of Rand so you cannot do that
// you can do that in the reverse direction.
// So the best way to do that is still to make Rand a type alias of State

// One trick here, notice that we have a val keyword in the default constructor
// if we don't have this val, run will not be a member of the classs.
case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- sb
    } yield f(a, b)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    }
  )
}

object State {

  type Rand[+A] = State[RNG, A]

  object Rand {
    val int: Rand[Int] = State(_.nextInt)

    def nonNegativeInt: Rand[Int] = 
      int.map(x => if (x < 0) -(x + 1) else x)
    
    def nonNegativeLessThan(n: Int): Rand[Int] = 
      nonNegativeInt.flatMap { i => 
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    
    def interval(start: Int, stopExclusive: Int): Rand[Int] = 
      nonNegativeLessThan(stopExclusive - start).map(_ + start)

    def double: Rand[Double] = 
      nonNegativeInt.map(_.toDouble / (Int.MaxValue.toDouble + 1))
  }
  // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  // create a state with a as output
  def unit[S, A](a: A): State[S, A] = 
    new State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = 
    fs.foldRight(unit[S, List[A]](List[A]()))((sa, sb) => sa.map2(sb)(_ :: _))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
// I cannot write it myself...
// See https://stackoverflow.com/questions/58587641/functional-programing-in-scala-exercise-6-11-how-does-this-for-comprehension-wo
// this answer for detailed explanation



