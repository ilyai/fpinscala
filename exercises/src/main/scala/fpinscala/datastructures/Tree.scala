package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) + 1).max(depth(r) + 1)
  }

  def depth2[A](t: Tree[A]): Int = fold(t)(a => 0)((a,b) => 1 + (a max b))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((a,b) => Branch(a,b))

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

}

object TreeTest {
  import Tree._

  def main(args: Array[String]) {
    val tree = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
    val tree2 = Branch(Leaf(2), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
    assert(size(tree) == 7)
    assert(maximum(tree) == 4)
    assert(depth(tree) == 3)
    assert(map(tree)(_ + 1) == tree2)
    assert(size2(tree) == 7)
    assert(maximum2(tree) == 4)
    assert(depth2(tree) == 3)
    assert(map2(tree)(_ + 1) == tree2)
  }
}