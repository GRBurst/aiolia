package aiolia.test

import aiolia.graph.DSL._
import aiolia.circuit.Circuit
import aiolia.test.Helpers._

class CircuitSpec extends org.specs2.mutable.Specification {
  //     ( )--> 0
  // --> ( )--> NOT
  // ->->( )--> NAND

  "only 1 output" >> {
    val c = Circuit(VL(), VL(0), graph(V(0), E()))
    c.compute(Array()) mustEqual Array(false)
    c.compile()
    c.compute(Array()) mustEqual Array(false)
  }

  "single NAND gate" >> {
    val c = Circuit(VL(0, 1), VL(2), graph(V(0, 1, 2, 3), E(0 -> 3, 1 -> 3, 3 -> 2)))
    c.compute(Array(false, false)) mustEqual Array(true)
    c.compute(Array(false, true)) mustEqual Array(true)
    c.compute(Array(true, false)) mustEqual Array(true)
    c.compute(Array(true, true)) mustEqual Array(false)
    c.compile()
    c.compute(Array(false, false)) mustEqual Array(true)
    c.compute(Array(false, true)) mustEqual Array(true)
    c.compute(Array(true, false)) mustEqual Array(true)
    c.compute(Array(true, true)) mustEqual Array(false)
  }

  "not gate" >> {
    val c = Circuit(VL(0), VL(1), graph(V(0, 1, 2), E(0 -> 2, 2 -> 1)))
    c.compute(Array(false)) mustEqual Array(true)
    c.compute(Array(true)) mustEqual Array(false)
    c.compile()
    c.compute(Array(false)) mustEqual Array(true)
    c.compute(Array(true)) mustEqual Array(false)
  }

  "two gates" >> {
    val c = Circuit(VL(0, 1), VL(3), graph(V(0, 1, 2, 3, 4), E(0 -> 2, 1 -> 2, 2 -> 4, 0 -> 4, 4 -> 3)))
    c.compute(Array(false, false)) mustEqual Array(true)
    c.compute(Array(false, true)) mustEqual Array(true)
    c.compute(Array(true, false)) mustEqual Array(false)
    c.compute(Array(true, true)) mustEqual Array(true)
    c.compile()
    c.compute(Array(false, false)) mustEqual Array(true)
    c.compute(Array(false, true)) mustEqual Array(true)
    c.compute(Array(true, false)) mustEqual Array(false)
    c.compute(Array(true, true)) mustEqual Array(true)
  }

  "insert subcircuit" >> {
    val c = Circuit(VL(0, 1), VL(3), graph(V(0, 1, 2, 3, 4), E(0 -> 2, 1 -> 2, 2 -> 4, 0 -> 4, 4 -> 3)))
    val sub = Circuit(VL(0, 1), VL(2), graph(V(0, 1, 2, 3), E(0 -> 3, 1 -> 3, 3 -> 2)))
    c.insertSubCircuit(VL(0, 2), VL(4), sub) mustEqual Circuit(VL(0, 1), VL(3), graph(V(0, 1, 2, 3, 4, 5), E(0 -> 2, 1 -> 2, 2 -> 4, 0 -> 4, 4 -> 3,
      0 -> 5, 2 -> 5, 5 -> 4)))
  }
}
