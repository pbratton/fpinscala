package fpinscala.datastructures

import scala.annotation.tailrec

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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x,xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Nil => Cons(h, Nil)
      case Cons(x,xs) => Cons(h, tail(l))
    }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x,xs) => if (n <= 0) l else drop(xs, n-1)
    }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs)(f) else l 
    }

  def init[A](l: List[A]): List[A] = 
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x: A, y: Int) => y + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }
  
  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Int]) = foldLeft(ns, 1)(_ * _)
  def length3(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + 1)
  
  def reverse[A](ns: List[A]) =
    foldLeft[A, List[A]](ns, Nil)((x, y) => Cons(y, x))
    
  def foldRight2[A,B](ns: List[A], z: B)(f: (B, A) => B): B =
    foldLeft[A,B](reverse(ns), z)((x,y) => f(x,y))
    
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)((x,y) => Cons(y, x))
    
  def concat[A](a: List[List[A]]): List[A] = 
    foldRight2[List[A], List[A]](a, Nil)((x,y) => append2(y, x))
    
  def add1(a1: List[Int]): List[Int] =
    foldRight2[Int, List[Int]](a1, Nil)((x,y) => Cons(y + 1, x))
  
  def doubleToString(ns: List[Double]): List[String] =
    foldRight2[Double, List[String]](ns, Nil)((x,y) => Cons(y.toString, x))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight2[A, List[B]](l, Nil)((x,y) => Cons(f(y), x))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2[A, List[A]](as, Nil)((x,y) => if (f(y)) Cons(y, x) else x)
    
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    concat(map(as)(f))
  
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)((x) => if (f(x)) List(x) else Nil)
  
  def vectorAdd(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Cons(x1, xs1), Nil) => Cons(x1, vectorAdd(xs1, a2))
      case (Nil, Cons(x2, xs2)) => Cons(x2, vectorAdd(a1, xs2))
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, vectorAdd(xs1, xs2))
    }
  
  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] =
    (a1, a2) match {
      case (Nil, Nil) => Nil
      case (Cons(x1, xs1), Nil) => Cons(x1, zipWith(xs1, a2)(f))
      case (Nil, Cons(x2, xs2)) => Cons(x2, zipWith(a1, xs2)(f))
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1,x2), zipWith(xs1, xs2)(f))
    }
}
