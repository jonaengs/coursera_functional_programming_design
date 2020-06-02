trait Generator[+T] {
  self =>  // alias for "this"

  def generate: T

  // Takes a value from the defined generator type T to some new type S
  // returns a new Generator with a generate function that calls returns f called on the current (T) generator through
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate)  // we can't write "generate" or "this.generate" because that would refer to the
                                        // new generate method that we are currently defining, not the one defined above.
                                        // This could also be solved by writing "Generator.this.generate"
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    // generates a value of type T, then calls f on that value, then calls generate on f's result
    // This works fine because f returns a generator of S-type values
    def generate: S = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt()
}

val booleans = for (x <- integers) yield  x > 0
// which expands to:
val booleans_exp = integers map (x => x > 0)
// which in turn expands to
val booleans_exp2 = new Generator[Boolean] {
  def generate: Boolean = integers.generate > 0
}


// so for pairs we get this:
def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = t flatMap {
  x => u map {y => (x, y)}
}
// which is simplified to:
def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = new Generator[(T, U)] {
  def generate: (T, U) = (t.generate, u.generate)
}


// building block Generator that simply returns the value that it is instantiated with
def single[T](x: T): Generator[T] = new Generator[T] {
  def generate: T = x
}
// Generates integers within some range (inclusive lower, exclusive upper)
def range(lo: Int, hi: Int): Generator[Int] =  // called "choose" in lecture
  for (x <- integers) yield lo + x % (hi - lo)
// Generates random choices from a list of arguments
def oneof[T](xs: T*): Generator[T] =
  for (idx <- range(0, xs.length)) yield xs(idx)



/*
  With the generators that we have defined above, we can now generate some more structured types:
*/
object listGen {
  // generates a random length list of random numbers
  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans  // 50/50-chance of returning empty list or not
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list
  // this requires the two functions below:
  def emptyLists: Generator[Nil.type] = single(Nil)
  def nonEmptyLists: Generator[List[Int]] = for {
      head <- integers
      tail <- lists
    } yield head :: tail
}
val l = listGen.lists.generate



/*
  Exercise: create generator for random Tree objects: */
trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

// My solution:
object treeGen {
  def trees: Generator[Tree] = for {
    end <- booleans
    node <- if (end) leafNodes else innerNodes
  } yield node
  def leafNodes: Generator[Leaf] = for (x <- integers) yield Leaf(x)
  def innerNodes: Generator[Inner] = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)

  def toString(tree: Tree): String = tree match {
    case Leaf(x) => x.toString
    case Inner(left, right) => "(" + toString(left) + ", " + toString(right) + ")"
  }
}
val t = treeGen.trees.generate
println(treeGen.toString(t))

/* Martin's solution. Exactly the same as mine, only different names :) */
object treeGenM {
  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree
}
treeGenM.trees.generate


/*
  A useful application of the generation of random data is in unit testing.
  Usually, we test by giving some input to some functions and checking against a postcondition (the expected result)

  The problem with this is that we only know for certain the outcome of the program for these specific inputs.
  We can generalize our testing by generating random values as our input. In some cases, this can be very useful.

  Below an example of this can be seen: */
def test[T](g: Generator[T], numTimes: Int = 100)
           (test: T => Boolean): Unit = {
  for (_ <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), "test "+test+" failed for: " + value)
  }
  println("passed "+numTimes+" tests")
}

def lists: Generator[List[Int]] = listGen.lists  // simply and alias for listGen.lists for brevity
test(pairs(lists, lists)) {  // intellij complains about this for some reason, but it runs just fine
  case (xs, ys) => (xs ++ ys).length > xs.length
}


/*
  ScalaCheck is a Scala implementation of a popular haskell testing library called QuickCheck
    - It can be used either standalone or together with ScalaTest

  QuickCheck-type libraries allow us to write properties that are assumed to hold instead of tests.

  ScalaCheck uses the function "forAll" to which we can pass values and some property that is assumed to hold
    - For many types, ScalaCheck will be able to generate test values on its own.
    - Additionally, these types of libraries will in some cases, when they find a non-passing value
      run the test on similar values, attempting to find a sort of "local optima" for the non-passing values.

Example:
forAll { (l1: List[Int], l2: List[Int]) =>
  l1.size + l2.size == (l1 ++ l2).size
}
 */

