package aiolia.test

import aiolia.NeuralNetwork
import aiolia.graph._
import aiolia.graph.dsl._

import Helpers._

//TODO: Rename to: RecurrentNeuralNetwork
class NeuralNetworkSpec extends org.specs2.mutable.Specification {
  "neuronal network" >> {
    "labels of vertices must be in order 0..|vertices|" >> {
      NeuralNetwork(in = Nil, out = Nil, Graph(V(1, 2), E(1 -> 2))) must throwAn[AssertionError]
    }

    "simple" >> {
      val n = NeuralNetwork(in = VL(0, 1), out = VL(2), Graph(V(0 to 2), E(0 -> 2, 1 -> 2), vData(2 -> -1), eData((0 -> 2) -> 0.4f, (1 -> 2) -> 0.6f)))
      n.setInputData(List(2, 4))
      n.think()
      n.outputData mustEqual List(0.9103665f)
    }

    "two steps" >> {
      val n = NeuralNetwork(in = VL(0, 1), out = VL(3), Graph(V(0 to 3), E(0 -> 2, 1 -> 2, 0 -> 3, 2 -> 3), vData(2 -> -1, 3 -> 2), eData((0 -> 2) -> 0.4f, (1 -> 2) -> 0.6f, (0 -> 3) -> 0.1f, (2 -> 3) -> 0.5f)))
      n.setInputData(List(2, 4))
      n.think()
      n.think()
      n.outputData mustEqual List(0.9358292f)
    }

    "fail on too much input data" >> {
      val n = NeuralNetwork(in = VL(0), out = VL(1), Graph(V(0 to 1), E(0 -> 1)))
      n.setInputData(List(0, 0)) must throwAn[AssertionError]
    }

    "fail on missing input data" >> {
      val n = NeuralNetwork(in = VL(0), out = VL(1), Graph(V(0 to 1), E(0 -> 1)))
      n.setInputData(Nil) must throwAn[AssertionError]
    }
  }
}
