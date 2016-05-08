package aiolia.test

import aiolia.FeedForwardNeuralNetwork
import aiolia.graph._
import aiolia.graph.dsl._

import Helpers._

class FeedForwardNeuralNetworkSpec extends org.specs2.mutable.Specification {
  def sigmoid(x: Double): Double = x / Math.sqrt(x * x + 1).toFloat
  "single neuron" >> {
    val n = FeedForwardNeuralNetwork(VL(0), VL(0), graph(V(0), E(), vData(0 -> 0.8)))
    n.compute(List(0.2)) mustEqual List(sigmoid(0.2 + 0.8))
  }
  "input and output not connected" >> {
    val n = FeedForwardNeuralNetwork(VL(0), VL(1), graph(V(0, 1), E(), vData(0 -> 0.8, 1 -> 2.1)))
    n.compute(List(0.2)) mustEqual List(sigmoid(2.1))
  }
  "one input -> one output" >> {
    val n = FeedForwardNeuralNetwork(VL(0), VL(1), graph(V(0, 1), E(0 -> 1),
      vData(0 -> 0.1, 1 -> 0.2),
      eData((0 -> 1) -> -0.7)))
    n.compute(List(0.7)) mustEqual List(sigmoid(sigmoid(0.7 + 0.1) * (-0.7) + 0.2))
  }
  "two inputs -> one output" >> {
    val n = FeedForwardNeuralNetwork(VL(0, 1), VL(2), graph(V(0, 1, 2), E(0 -> 2, 1 -> 2),
      vData(0 -> 0.1, 1 -> 0.2, 2 -> -0.9),
      eData((0 -> 2) -> -0.7, (1 -> 2) -> 0.4)))
    n.compute(List(0.7, 0.8)) mustEqual List(sigmoid(sigmoid(0.7 + 0.1) * (-0.7) + sigmoid(0.8 + 0.2) * 0.4 + (-0.9)))
  }
  "skipping one layer" >> {
    // vertex 2 is skipped layer
    val n = FeedForwardNeuralNetwork(VL(0, 1), VL(3), graph(V(0, 1, 2, 3), E(0 -> 2, 1 -> 2, 2 -> 3, 0 -> 3),
      vData(0 -> 0.1, 1 -> 0.2, 2 -> -0.9, 3 -> -0.2),
      eData((0 -> 2) -> -0.7, (1 -> 2) -> 0.4, (2 -> 3) -> 1.1, (0 -> 3) -> -0.1)))
    val v0 = sigmoid(0.7 + 0.1)
    val v1 = sigmoid(0.8 + 0.2)
    val v2 = sigmoid(v0 * (-0.7) + v1 * 0.4 + (-0.9))
    val v3 = sigmoid(v2 * 1.1 + v0 * (-0.1) + (-0.2))
    n.compute(List(0.7, 0.8)) mustEqual List(v3)
  }
}
