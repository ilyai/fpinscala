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

  def exists2(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists2(p)
    case _ => false
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case _ if n == 0 => Empty
    case Empty => Empty
    case Cons(h,t) => cons(h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a,b) => if (p(a)) cons(a, b) else Empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }

  def headOption2: Option[A] = foldRight(None: Option[A])((a,b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a,b) => cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a,b) => if (p(a)) cons(a, b) else b)
  def append[T >: A](s: Stream[T]): Stream[T] = foldRight(s)((a,b) => cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a,b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] = this match {
    case Empty => Nil: List[A]
    case Cons(h,t) => h() :: t().toList
  }
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

  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = cons(a, s)
    s
  }
  def constant2[A](a: A): Stream[A] = cons(a, constant2(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs[A]: Stream[Int] = {
    def loop(p: Int, n: Int): Stream[Int] = cons(p + n, loop(p + n, p))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}

object StreamTest {
  import Stream._

  def main(args: Array[String]): Unit = {
    assert(cons(1, cons(2, cons(3, Empty))).toList == List(1,2,3))
    assert(cons(1, cons(2, cons(3, Empty))).take(2).toList == List(1,2))
    assert(cons(1, cons(2, cons(3, Empty))).drop(2).toList == List(3))
    assert(cons(1, cons(2, cons(3, Empty))).takeWhile(_ <= 2).toList == List(1,2))
    assert(cons(1, cons(2, cons(3, Empty))).takeWhile2(_ <= 2).toList == List(1,2))
    assert(cons(1, cons(2, cons(3, Empty))).forAll(_ < 5))
    assert(!cons(1, cons(2, cons(3, Empty))).forAll(_ < 2))
    assert(cons(1, cons(2, cons(3, Empty))).headOption2.contains(1))
    assert(!Empty.headOption2.contains(1))
    assert(cons(1, cons(2, cons(3, Empty))).map(_ * 2).toList == List(2,4,6))
    assert(cons(1, cons(2, cons(3, Empty))).filter(_ < 3).toList == List(1,2))
    assert(cons(1, cons(2, cons(3, Empty))).append(cons(4, cons(5, Empty))).toList == List(1,2,3,4,5))
    assert(cons(1, cons(2, cons(3, Empty))).flatMap(a => cons(a, cons(a, Empty))).toList == List(1,1,2,2,3,3))
    assert(constant(1).take(3).toList == List(1,1,1))
    assert(constant2(1).take(3).toList == List(1,1,1))
    assert(from(1).take(3).toList == List(1,2,3))
    assert(fibs.take(3).toList == List(1,1,2))
  }
}