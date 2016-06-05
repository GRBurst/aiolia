package aiolia.test

import aiolia.graph.DSL._
import aiolia.graph._
import aiolia.neuralNetwork.Recurrent

class RecurrentNeuralNetworkSpec extends org.specs2.mutable.Specification {
  "neuronal network" >> {
    "labels of vertices must be in order 0..|vertices|" >> {
      Recurrent(in = Nil, out = Nil, Graph(V(1, 2), E(1 -> 2))) must throwAn[AssertionError]
    }

    "simple" >> {
      val n = Recurrent(in = VL(0, 1), out = VL(2), Graph(V(0 to 2), E(0 -> 2, 1 -> 2), vData(2 -> -1.0).withDefaultValue(0.0), eData((0 -> 2) -> 0.4, (1 -> 2) -> 0.6).withDefaultValue(0.0)))
      n.setInputData(List(2, 4))
      n.think()
      n.outputData mustEqual List(0.9103664774626048)
    }

    "two steps" >> {
      val n = Recurrent(in = VL(0, 1), out = VL(3), Graph(V(0 to 3), E(0 -> 2, 1 -> 2, 0 -> 3, 2 -> 3), vData(2 -> -1.0, 3 -> 2.0).withDefaultValue(0.0), eData((0 -> 2) -> 0.4, (1 -> 2) -> 0.6, (0 -> 3) -> 0.1, (2 -> 3) -> 0.5).withDefaultValue(0.0)))
      n.setInputData(List(2, 4))
      n.think()
      n.think()
      n.outputData mustEqual List(0.9358292403716548)
    }

    "fail on too much input data" >> {
      val n = Recurrent(in = VL(0), out = VL(1), Graph(V(0 to 1), E(0 -> 1), vData().withDefaultValue(0.0), eData().withDefaultValue(0.0)))
      n.setInputData(List(0, 0)) must throwAn[AssertionError]
    }

    "fail on missing input data" >> {
      val n = Recurrent(in = VL(0), out = VL(1), Graph(V(0 to 1), E(0 -> 1), vData().withDefaultValue(0.0), eData().withDefaultValue(0.0)))
      n.setInputData(Nil) must throwAn[AssertionError]
    }
  }
}
