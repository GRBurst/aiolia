package aiolia.test

import aiolia.util.Random

class RandomSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {
  def randMock = {
    val rand = spy(new Random(seed = 0))
    rand.r returns mock[util.Random]
    rand
  }

  "random" >> {
    "select item from iterable collection" >> {

      "empty collection" >> {
        Random(13).select(Set.empty[Int]) must throwAn[Exception]
      }

      "one element" >> {
        Random(11).select(Set(1)) mustEqual 1
      }

      "first element" >> {
        val rand = randMock
        rand.r.nextInt(5) returns 0
        rand.select(List(1, 2, 3, 4, 5)) mustEqual 1
      }

      "last element" >> {
        val rand = randMock
        rand.r.nextInt(5) returns 4
        rand.select(List(1, 2, 3, 4, 5)) mustEqual 5
      }

      "middle element" >> {
        val rand = randMock
        rand.r.nextInt(5) returns 2
        rand.select(List(1, 2, 3, 4, 5)) mustEqual 3
      }

      "multiple elements" >> {
        val rand = new Random(seed = 0)
        val l = List(1, 2, 3, 4, 5)
        val r = rand.select(l, 3)
        r.size mustEqual 3

        //TODO: make it work with mock?
        // val rand = randMock
        // val l = List(1, 2, 3, 4, 5)
        // rand.r.shuffle(l)(collection.immutable.List.canBuildFrom[Int]) returns l
        // rand.select(l, 3) mustEqual List(1, 2, 3)
      }
    }
  }
}
