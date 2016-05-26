val eq = (x: Int, y: Int) => x == y

val lessThan = new Function2[Int, Int, Boolean] {
  def apply(a: Int, b: Int): Boolean = a < b
}

val b = lessThan.apply(10, 20)

def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

val f = (x: Double) => math.Pi / 2 - x
val cos = f.andThen(math.sin)