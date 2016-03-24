package aiolia.neuralnetwork

import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraphgrammar._


object Helpers {
  implicit def VertexTupleToEdge(tuple: (Int, Int)) = Edge(Vertex(tuple._1), Vertex(tuple._2))
  implicit def IntToVertex(i: Int) = Vertex(i)
  def vertexData[V](data: (Int, V)*) = data.map { case (label, data) => Vertex(label) -> data }.toMap
  def edgeData[E](data: ((Int, Int), E)*) = data.map { case ((a, b), data) => Edge(a, b) -> data }.toMap
}

import Helpers._

class NeuralNetworkSpec extends org.specs2.mutable.Specification {
  "neuronal network" >> {
    "simple" >> {
      val n = NeuralNetwork(in = List(0, 1), out = List(2), Graph(Set(0 -> 2, 1 -> 2), vertexData(2 -> -1), edgeData((0 -> 2) -> 0.4f, (1 -> 2) -> 0.6f)))
      n.setInputData(List(2, 4))
      n.think()
      n.outputData mustEqual List(0.9103665f)
    }

    "two steps" >> {
      val n = NeuralNetwork(in = List(0, 1), out = List(3), Graph(Set(0 -> 2, 1 -> 2, 0 -> 3, 2 -> 3), vertexData(2 -> -1, 3 -> 2), edgeData((0 -> 2) -> 0.4f, (1 -> 2) -> 0.6f, (0 -> 3) -> 0.1f, (2 -> 3) -> 0.5f)))
      n.setInputData(List(2, 4))
      n.think()
      n.think()
      n.outputData mustEqual List(0.9358292f)
    }

    "fail on too much input data" >> {
      val n = NeuralNetwork(in = List(0), out = List(1), Graph(Set(0 -> 1)))
      n.setInputData(List(0, 0)) must throwAn[AssertionError]
    }

    "fail on missing input data" >> {
      val n = NeuralNetwork(in = List(0), out = List(1), Graph(Set(0 -> 1)))
      n.setInputData(List.empty) must throwAn[AssertionError]
    }
  }
}
