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

  def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

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

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a,b) => Cons(a, b))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 1) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs)(f)
      else l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,a) => a + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)((b,a) => f(a,b))      // ???
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((a,b) => f(b,a))    // ???

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((a, b) => Cons(f(a), b))

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((a,b) => append2(a, b))

  def add1(xs: List[Int]): List[Int] = map(xs)(_ + 1)

  def doublesToStrings(xs: List[Double]): List[String] = map(xs)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((a,b) => if (f(a)) Cons(a,b) else b)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil:List[B])((a, b) => append2(f(a), b))

  def zip[A](a1: List[A], a2: List[A]): List[List[A]] = (a1,a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(List(h1,h2), zip(t1, t2))
  }

  def zipAndSum(a1: List[Int], a2: List[Int]): List[Int] = (a1,a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1 + h2, zipAndSum(t1, t2))
  }

  def zipWith[A,B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] = (a1,a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def matches(haystack: List[A], needle: List[A]): Boolean = (haystack, needle) match {
      case (_, Nil) => true
      case (Cons(h1,t1), Cons(h2,t2)) => if (h1 == h2) matches(t1, t2) else false
      case _ => false
    }
    (sup, sub) match {
      case (Nil, _) => false
      case (_, Nil) => false
      case (Cons(h1,t1), Cons(h2,t2)) => if (h1 == h2) {
        if (matches(t1, t2)) true
        else hasSubsequence(t1, sub)
      } else hasSubsequence(t1, sub)
    }
  }
}

object ListTest {
  import List._

  def main(args: Array[String]): Unit = {
    val l = List(1,2,3)
    assert(x == 3)
    assert(tail(l) == List(2, 3))
    assert(setHead(l, 0) == List(0, 2, 3))
    assert(drop(l, 2) == List(3))
    assert(dropWhile(l)(_ < 3) == List(3))
    assert(init(l) == List(1,2))
    assert(product(List(3,2)) == 6)
    assert(foldRight(l, Nil:List[Int])(Cons(_,_)) == l)
    assert(length(l) == 3)
    assert(sum3(l) == 6)
    assert(product3(List(1.0, 2.0, 3.0)) == 6.0)
    assert(reverse(l) == List(3,2,1))
    assert(foldLeft2(l, 0)(_ + _) == 6)
    assert(foldRight2(l, 0)(_ + _) == 6)
    assert(append2(l, List(4,5)) == List(1,2,3,4,5))
    assert(map(l)(_ + 1) == List(2,3,4))
    assert(flatten(List(List(1,2),List(3,4))) == List(1,2,3,4))
    assert(add1(l) == List(2,3,4))
    assert(doublesToStrings(List(1.0,2.0)) == List("1.0", "2.0"))
    assert(filter(l)(_ % 2 == 0) == List(2))
    assert(flatMap(l)(i => List(i,i)) == List(1,1,2,2,3,3))
    assert(filter2(l)(_ % 2 == 0) == List(2))
    assert(zipAndSum(l, List(4,5,6)) == List(5,7,9))
    assert(zipWith(l, List(4,5,6))(_ * _) == List(4, 10, 18))
    assert(hasSubsequence(l, List(1,2)) == true)
    assert(hasSubsequence(l, List(2)) == true)
    assert(hasSubsequence(l, List(3,2)) == false)
  }
}