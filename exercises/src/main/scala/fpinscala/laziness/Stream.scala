package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = 
    foldRight[List[A]](Nil)((h, t) => h :: t) // only tolist will force instantiate, all arguments are lazy except for the list. no calculation will be done if we are only dealing with stream
  
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }


  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case c @ Cons(_, _) if n == 0 => c
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty) // cannot use Cons(h, t)
  
  def takeWhile2(p: A => Boolean): Stream[A] = 
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None 
    }

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((h, t) => p(h) && t)

  def headOption: Option[A] = 
    foldRight[Option[A]](None)((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = 
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def map2[B](f: A => B): Stream[B] = 
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  
  def filter(f: A => Boolean): Stream[A] =  
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  
  def append[B>:A](a: => Stream[B]): Stream[B] = 
    foldRight(a)((h, t) => cons(h, t)) // [B>:A] is a must, something related to covariant and contracovariant, don't know
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((h, t) => f(h).append(t))
  
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = 
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean = 
    this.zipAll(s).forAll {
      case (Some(a), Some(b)) => a == b
      case (None, Some(_)) => false
      case _ => true
    }
  
  def tails: Stream[Stream[A]] = 
    unfold(this) {
      case c @ Cons(h, t) => Some((c, t()))
      case _ => None
    } append Stream(empty)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs = {
    def go(f1: Int, f2: Int): Stream[Int] = 
      cons(f1, go(f2, f1 + f2))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A] 
  }

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def from2(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  // pattern matching anonymous function, cannot use () but {}
  // note that if we use pattern matching to define a function, then match can be omitted (I didn't find anything explaining that, just my own reasoning)
  val fibs2 = unfold((0, 1)){ case (f1, f2) => Some(f1, (f2, f1 + f2))} 
}