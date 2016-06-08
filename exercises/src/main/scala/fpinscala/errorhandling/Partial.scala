package fpinscala.errorhandling

trait Partial[+A,+B] {
  def map[C](f: B => C): Partial[A, C] = this match {
    case Errors(e) => Errors(e)
    case Success(a) => Success(f(a))
  }

  def flatMap[AA >: A, C](f: B => Partial[AA, C]): Partial[AA, C] = this match {
    case Errors(e) => Errors(e)
    case Success(a) => f(a)
  }

  def map2[AA >: A, C, D](b: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] = this.flatMap(a => b.map(b => f(a,b)))
}
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Partial[String, Name] = {
    if (name == "" || name == null) Errors(Seq("Name is empty."))
    else Success(new Name(name))
  }

  def mkAge(age: Int): Partial[String, Age] = {
    if (age < 0) Errors(Seq("Age is out of range."))
    else Success(new Age(age))
  }

  def mkPerson(name: String, age: Int): Partial[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_, _))
  }
}

object PersonTest {
  import Person._

  def main(args: Array[String]): Unit = {
    assert(mkPerson("", -1) == Errors(List("Name is empty.", "Age is out of range.")))  // FIXME
  }
}