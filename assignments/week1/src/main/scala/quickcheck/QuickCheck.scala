package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // the two "default" properties
  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }


  // insert two elems. findMin should always find the smallest of the two
  property("insert_2_find_min") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    if (a <= b) findMin(h) == a
    else findMin(h) == b
  }

  // inserting into an empty, then deleting the min should yield an empty heap
  property("insert_then_del_yields_empty") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    h == empty
  }

  // elements found by findMin should be sorted
  @tailrec
  private def minIsSorted(h: H, prevMin: Int): Boolean = {  // goes through heap, checks that findMin is always growing
    if (h == empty) true
    else {
      val newMin = findMin(h)
      if (prevMin <= newMin) minIsSorted(deleteMin(h), newMin) else false
    }
  }
  property("findMin_sorted") = forAll { h: H =>
    if (h != empty) minIsSorted(h, findMin(h))
    else true
  }

  // minimum of two melded heaps should be minimum of one of the two
  property("findMin_meld_kept") = forAll{ (h1: H, h2: H) =>
    if (h1 == empty || h2 == empty) true
    else {
      val min = findMin(meld(h1, h2))
      min == findMin(h1) || min == findMin(h2)
    }
  }

  // isEmpty means actually empty
  property("empty") = forAll{ h: H =>
    if (isEmpty(h)) h == empty
    else h != empty
  }

  // all elements found in either heap should be found in melded heap
  def meldMinInHeaps(m: H, h1: H, h2: H): Boolean = {
    if (m == empty) true
    else {
      val min = findMin(m)
      if (h1 != empty && findMin(h1) == min)
        meldMinInHeaps(deleteMin(m), deleteMin(h1), h2)
      else if (h2 != empty && findMin(h2) == min)
        meldMinInHeaps(deleteMin(m), h1, deleteMin(h2))
      else false
    }
  }
  property("meld_keeps_elems") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    meldMinInHeaps(m, h1, h2)
  }

  // inserting a new min makes findMin return that value
  property("insert_new_min") = forAll{ h: H =>
    if (isEmpty(h)) true
    else {
      val currentMin = findMin(h)
      if (currentMin == Int.MinValue) true
      else findMin(insert(currentMin - 1, h)) < currentMin
    }
  }

  // inserting a value above min means that value is not returned by findMin
  property("insert_not_new_min") = forAll{ h: H =>
    if (isEmpty(h)) true
    else {
      val currentMin = findMin(h)
      if (currentMin == Int.MaxValue)
        true
      else
        currentMin == findMin(insert(currentMin + 1, h)) // insert plus one. Should get old min back as min
    }
  }

}
