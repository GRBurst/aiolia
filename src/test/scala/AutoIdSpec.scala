package aiolia.test

import aiolia.helpers.AutoId

class AutoIdSpec extends org.specs2.mutable.Specification {
  "increment" >> {
    val auto = AutoId(5)
    auto.nextId mustEqual 5
    auto.nextId mustEqual 6
    auto.nextId mustEqual 7
    auto.nextId mustEqual 8
  }
}
