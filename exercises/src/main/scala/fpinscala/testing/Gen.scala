package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase,SuccessCount),SuccessCount]
//  def &&(p: Prop): Prop = new Prop {
//    def check = Prop.this.check && p.check
//  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
//  def listOf[A](a: Gen[A]): Gen[List[A]]
//  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
//  def choose(start: Int, stopExclusive: Int): Gen[Int] =
//    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

//case class Gen[A](sample: State[RNG, A])

trait SGen[+A] {

}

object PropTest {
  import Prop._
  import Gen._

  def main(args: Array[String]): Unit = {

  }
}
