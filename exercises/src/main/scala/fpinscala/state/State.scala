package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,rng2) = rng.nextInt
    (math.abs(i), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i,rng2) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  def double2: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1,rng2) = nonNegativeInt(rng)
    val (d1,rng3) = double(rng2)
    ((i1,d1), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i1,d1), rng2) = intDouble(rng)
    ((d1,i1), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    val (d3,rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count <= 0) (xs, r)
      else {
        val (i,r2) = r.nextInt
        go(count - 1, r2, i :: xs)
      }
    }
    go(count, rng, Nil)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((a,acc) => map2(a, acc)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      unit(mod)
    else nonNegativeLessThan(n)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
}

object RNGTest {
  import RNG._

  def main(args: Array[String]): Unit = {
    val rng = Simple(42)
    assert(randomPair(rng)._1 == (16159453,-1281479697))
    assert(nonNegativeInt(rng)._1 > 0)
    assert(double(rng)._1.isInstanceOf[Double])
    assert(intDouble(rng)._1 == (16159453,-0.5967354853637516))
    assert(doubleInt(rng)._1 == (-0.5967354853637516,16159453))
    assert(double3(rng)._1 == (0.007524831686168909,-0.5967354853637516,-0.15846728440374136))
    assert(ints(3)(rng)._1 == List(-340305902, -1281479697, 16159453))
    assert(double2(rng)._1 == 0.007524831686168909)
    assert(randIntDouble(rng)._1 == (16159453,-0.5967354853637516))
    assert(randDoubleInt(rng)._1 == (0.007524831686168909,-1281479697))
    assert(sequence(List(double2, double2))(rng)._1 == List(0.007524831686168909, 0.5967354853637516))
    assert(nonNegativeLessThan(5)(rng)._1 == 3)
    assert(nonNegativeLessThan2(5)(rng)._1 == 3)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}