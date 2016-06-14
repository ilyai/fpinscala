var rng = new scala.util.Random

rng.nextDouble()
rng.nextDouble()
rng.nextInt()
rng.nextInt(10)

def rollDie: Int = {
  var rng = new scala.util.Random
  rng.nextInt(6)
}

