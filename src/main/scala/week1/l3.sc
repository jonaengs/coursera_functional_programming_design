trait Generator[+T] {
  def generate: T
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

val booleans = new Generator[Boolean] {
  def generate: Boolean = integers.generate > 0
}

val pairs = new Generator[(Int, Int)] {
  def generate: (Int, Int) = (integers.generate, integers.generate)
}

/*
 And so on and so forth for every other generator we may want.
 This is pretty cumbersome though. A better solution would be to to something like
    val booleans = for (x <- integers) yield x > 0
    def pairs[T, U](t: Generator[T], u: Generator[U]) = for {  // generates any pair
      x <- t
      y <- u
    } yield (x, y)
 This last one expands to: t flatMap (x => u map(y => (x, y))
 which means that if we implement map and flatMap on our generators, we will be able to use for-expressions like this.
 We will now do this in the scratch l3b.sc


 */