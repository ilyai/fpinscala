List(1,2,3,4).map(_ + 10).filter(_ % 2 ==0).map(_ * 3)
List(11,12,13,14).filter(_ % 2 ==0).map(_ * 3)
List(12,14).map(_ * 3)
List(36,42)

// strict function
def square(x: Double) = x * x
square(41.0 + 1.0)
//square(sys.error("failure"))

// lazy function
def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
  if (cond) onTrue else onFalse
}

val a = 2
if2(a < 22,
  println("a"),
  println("b"))

if2(false, sys.error("fail"), 3)

def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0
val x = maybeTwice(true, { println("hi"); 1 + 41 })


def maybeTwice2(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j+j else 0
}
val y = maybeTwice2(true, { println("hi"); 1 + 41 })