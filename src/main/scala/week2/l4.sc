def from(n: Int): LazyList[Int] = n #:: from(n + 1)

val nats = from(0)
val m4s = nats map (_ * 4)


def sieve(s: LazyList[Int]): LazyList[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))
primes.take(100).toList

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double): Double = (guess + x/guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

sqrtStream(2).take(10).toList

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess*guess - x) / x) < 0.0001

sqrtStream(4).filter(isGoodEnough(_, 4)).take(1).toList
