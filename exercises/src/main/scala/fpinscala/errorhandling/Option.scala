package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter\
import scala.math

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(get) => get
  }

  // if value exists, pass the value to the function you assign and get the result
  def flatMap[B](f: A => Option[B]): Option[B] = 
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = 
    flatMap(get => if (f(get)) Some(get) else None)
    
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap (aa => b flatMap (bb => Some(f(aa, bb))))

  // for comprehension, see https://docs.scala-lang.org/tutorials/FAQ/yield.html
  // for(x <- c1; y <- c2; z <- c3) yield {...}
  // is translated into
  // c1.flatMap(x => c2.flatMap(y => c3.map(z => {...})))
  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    // a.foldRight[Option[A]] (Some(Nil)) ((x, y) => map2(x, y)(x :: y))
    a.foldRight(Some(Nil): Option[List[A]])((x, y) => map2(x, y)(_ :: _))
  
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a.foldRight(Some(Nil): Option[List[B]])((x, y) => map2(f(x), y)(_ :: _))
  
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}