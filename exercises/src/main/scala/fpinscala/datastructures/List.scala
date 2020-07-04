package fpinscala.datastructures

import scala.collection.immutable.Stream.cons

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  // same as reduce, z is the initial value, starting from the last element
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f)) // not tail recursive, f() is the last function called
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(head, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case x if x <= 0 => l
    case _ => l match {
      case Nil => Nil
      case Cons(head, tail) => drop(tail, n - 1)
    }
  }
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else l 
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, x) => x + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  def sum3(l: List[Int]): Int = 
    foldLeft(l, 0)(_ + _)
  
  def product3(l: List[Double]): Double = 
    foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = 
    foldLeft(l, 0)((x, _) => x + 1)
  
  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil: List[A])((xs, x) => Cons(x, xs)) // cannot just write nil, nil is not type list, need type cast?
  
  def append2[A](l: List[A], r: List[A]) = 
    foldRight(l, r)(Cons(_, _))
  
  def concat[A](l: List[List[A]]) = 
    foldRight(l, Nil: List[A])(append2)
  
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail)) // not tail recursive
  }

  def init2[A](l: List[A]): List[A] = {
    var buffer = List[A]() // using var is not pure functional, use listbuffer instead.
    @annotation.tailrec
    def helper(l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => reverse(buffer)
      case Cons(head, tail) => buffer = Cons(head, buffer); helper(tail)
    }
    helper(l)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, Nil: List[B])((head, tail) => Cons(f(head), tail))
  
  def map2[A,B](l: List[A])(f: A => B): List[B] = {
    var buffer = List[B]()
    @annotation.tailrec
    def helper(l: List[A]): List[B] = l match {
      case Nil => reverse(buffer)
      case Cons(head, tail) => buffer = Cons(f(head), buffer); helper(tail)
    }
    helper(l)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = 
    foldRight(l, Nil: List[A])((head, tail) => if (f(head)) Cons(head, tail) else tail)
  
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = 
    concat(map2(l)(f))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = 
    flatMap(l)(x => if (f(x)) List(x) else Nil)
  
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}
