package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
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

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(av => b.map(bv => f(av, bv)))

  def map22[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map22(x, y)(_ :: _))

  def parseInts(a: List[String]): Option[List[Int]] = sequence(a.map(i => Try(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((x,y) => map2(f(x),y)(_ :: _))
  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((x,y) => map22(f(x),y)(_ :: _))

  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  /**
    * Top secret formula from computing an annual car
    * insurance premium from two key factors.
    * @param age
    * @param numberOfSpeedingTickets
    * @return
    */
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets

  def parseInsuranceRateQuote(
                             age: String,
                             numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None }
}

object OptionTest {
  import Option._

  def main(args: Array[String]) {
    val opt = Some("foo")
    assert(opt.map(_ + "bar") == Some("foobar"))
    assert(opt.getOrElse("quux") == "foo")
    assert(Some(Some("foobar")).flatMap(a => a) == Some("foobar"))
    assert(opt.orElse(Some("baz")) == opt)
    assert(opt.filter(_ == "foobar") == None)
    assert(opt.filter(_ == "foo") == Some("foo"))
    assert(variance(Seq(2.0, 2.0, 2.0)) == Some(0.0))
    assert(parseInsuranceRateQuote("21", "4") == Some(84.0))
    assert(sequence(List(Some("foo"), Some("bar"))) == Some(List("foo", "bar")))
    assert(traverse(List("3", "2"))(i => Try(i.toInt)) == Some(List(3, 2)))
    assert(sequence2(List(Some("foo"), Some("bar"))) == Some(List("foo", "bar")))
    assert(traverse2(List("3", "2"))(i => Try(i.toInt)) == Some(List(3, 2)))
  }
}