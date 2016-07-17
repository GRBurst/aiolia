package aiolia.test

import aiolia.graph.DSL._
import aiolia.test.Helpers._

import aiolia.circuit.Circuit
import aiolia.circuit.Simplification._

class CircuitSimplificationSpec extends org.specs2.mutable.Specification {
  "remove simple inversion" >> {
    val c = Circuit(VL(0), VL(3), graph(V(0, 1, 2, 3), E(0 -> 1, 1 -> 2, 2 -> 3)))
    removeDoubleInversion(c.graph) mustEqual graph(V(0, 3), E(0 -> 3))
  }

  def sameLogic(circuitA: Circuit, circuitB: Circuit): Boolean = {
    assert(circuitA.in.size == circuitB.in.size)
    assert(circuitA.out.size == circuitB.out.size)
    val inputSize = circuitA.in.size
    inputExamples(inputSize).map(_.toArray).forall(e => circuitA.compute(e).toSeq == circuitB.compute(e).toSeq)
  }

  "same logic" >> {
    val a = Circuit(VL(0), VL(3), graph(V(0, 1, 2, 3), E(0 -> 1, 1 -> 2, 2 -> 3)))
    val b = Circuit(VL(0), VL(1), graph(V(0, 1), E(0 -> 1)))
    sameLogic(a, b) must beTrue
  }

  "same logic" >> {
    val a = Circuit(VL(0), VL(3), graph(V(0, 1, 2, 3), E(0 -> 1, 1 -> 2, 2 -> 3)))
    val b = Circuit(VL(0), VL(2), graph(V(0, 1, 2), E(0 -> 1, 1 -> 2)))
    sameLogic(a, b) must beFalse
  }

  def inputExamples(n: Int): Seq[Seq[Boolean]] = {
    assert(n >= 1)
    if (n == 1) List(Array(false), Array(true))
    else inputExamples(n - 1).map(false +: _) ++ inputExamples(n - 1).map(true +: _)
  }

  "input examples" >> {
    inputExamples(1) mustEqual Seq(
      Seq(false),
      Seq(true)
    )
    inputExamples(2) mustEqual Seq(
      Seq(false, false),
      Seq(false, true),
      Seq(true, false),
      Seq(true, true)
    )
    inputExamples(3) mustEqual Seq(
      Seq(false, false, false),
      Seq(false, false, true),
      Seq(false, true, false),
      Seq(false, true, true),
      Seq(true, false, false),
      Seq(true, false, true),
      Seq(true, true, false),
      Seq(true, true, true)
    )
  }

}
