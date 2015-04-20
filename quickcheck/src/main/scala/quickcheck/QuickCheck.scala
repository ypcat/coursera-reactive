package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min") = forAll(Gen.choose(0, 100)){ a =>
    val h1 = insert(a + 1, insert(a + 2, insert(a, empty)))
    val h2 = insert(a + 2, insert(a, insert(a - 1, empty)))
    a == findMin(meld(h1, deleteMin(h2)))
  }

  property("min2") = forAll(Gen.choose(0, 100)){ a =>
    a == findMin(insert(a + 1, insert(a, empty)))
  }

  property("delete") = forAll(Gen.choose(0, 100)){ a =>
    a == findMin(deleteMin(insert(a + 1, insert(a - 1, insert(a, empty)))))
  }

  property("meld") = forAll(Gen.choose(0, 100)){ a =>
    val h1 = insert(a + 1, insert(a + 2, empty))
    val h2 = insert(a + 2, insert(a, empty))
    a == findMin(meld(h1, h2))
  }

  lazy val genHeap: Gen[H] = ???

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
