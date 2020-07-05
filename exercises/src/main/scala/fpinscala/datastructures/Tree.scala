package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
        case Leaf(value) => f(value)
        case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
    
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(left, right) => size(left) + size(right) + 1
    }

    def size2[A](t: Tree[A]): Int = 
        fold(t)(_ => 1)(_ + _ + 1)

    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(value) => value
        case Branch(left, right) => maximum(left) max maximum(right)
    }

    def maximum2(t: Tree[Int]): Int = 
        fold(t)(x => x)(_ max _)


    def depth[A](t: Tree[A]): Int = 
        fold(t)(_ => 1)(_ max _ + 1)
    
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = 
        fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _)) // : Tree[B] type annotation is a must, otherwise scala will resolve the subtype Leaf[B], which will casue a type mismatch in Branch(_, _), see answers
}