package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right)
    }
  
  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  
  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => depth(left) max depth(right) + 1
    }
  
  def map[A,B](tree: Tree[A])(f: (A) => B): Tree[B] =
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f),map(right)(f))
    }

  def fold[A,B](tree: Tree[A])(f: (A) => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g),fold(right)(f)(g))
    }
  }
  
  def size2[A](tree: Tree[A]) = fold(tree)((_) => 1)(_ + _)
  def maximum2(tree: Tree[Int]) = fold(tree)((x) => x)(_ max _)
  def depth2[A](tree: Tree[A]) = fold(tree)((_) => 1)(_ max _ + 1)
  def map2[A,B](tree: Tree[A])(f: (A) => B): Tree[B] = 
    fold[A,Tree[B]](tree)((x) => Leaf(f(x)))((b1,b2) => Branch(b1,b2)) 
  


}