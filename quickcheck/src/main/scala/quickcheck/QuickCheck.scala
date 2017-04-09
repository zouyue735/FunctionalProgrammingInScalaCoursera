package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.collection.immutable.SortedSet

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty), for {
      i <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("emptyness") = {
    isEmpty(empty)
  }

  property("non-emptyness") = forAll { (h: H, i: A) =>
    !isEmpty(insert(i, h))
  }

  property("sorting") = forAll {
        val a = 1
    (h: H) =>
      println(a)
    val list = toList(h)
    list.equals(list.sorted)
  }

  property("insert") = forAll { (ints: List[A], h: H) =>
    val hs = insertList(h, ints)
    val list = toList(h)
    val lists = toList(hs)
    lists.equals((list ++ ints).sorted)
  }

  property("meld-emptyness") = forAll { (h1: H, h2: H) =>
    val hm = meld(h1, h2)
    isEmpty(hm).equals(isEmpty(h1) && isEmpty(h2))
  }

  property("meld-sorting") = forAll { (h1: H, h2: H) =>
    val l1 = toList(h1)
    val l2 = toList(h2)
    val lm = toList(meld(h1, h2))
    lm.equals(lm.sorted) && lm.equals((l1 ++ l2).sorted)
  }

  def insertList(h: H, list: List[A]): H = list match {
    case Nil => h
    case head :: tail => insertList(insert(head, h), tail)
  }

  def toList(h: H): List[A] = {
    if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))
  }
}