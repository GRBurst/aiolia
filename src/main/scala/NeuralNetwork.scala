package aiolia.neuralnetwork

import aiolia.graph._
import aiolia.hypergraphgrammar._

import collection.mutable

object NeuralNetwork {
  def apply(in: List[Vertex], out: List[Vertex], graph: Graph[Float, Float]) = {
    new NeuralNetwork(in, out, graph)
  }
}

class NeuralNetwork(in: List[Vertex], out: List[Vertex], graph: Graph[Float, Float]) {
  // Important for data indexing in neural network
  assert((0 until graph.vertices.size).forall(graph.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${graph.vertices}")

  assert(in.forall(graph.incomingEdges(_).isEmpty), "Input neurons should not have incoming edges")
  assert(out.forall(graph.outgoingEdges(_).isEmpty), "Output neurons should not have outgoing edges")

  import graph.{vertices => neurons}
  import graph.{vertexData => bias}
  import graph.{edgeData => weight}

  private var currentState = 0
  private val stateArray = Array(Array.fill[Float](graph.vertices.size)(0), Array.fill[Float](graph.vertices.size)(0))
  private def swapStates() { currentState = 1 - currentState }
  def state = stateArray(currentState)
  def prevState = stateArray(1 - currentState)

  def setInputData(data: List[Float]) { //TODO: iterable?
    assert(in.size == data.size, "Need to set all input data")

    for ((neuron, datum) <- (in zip data)) {
      prevState(neuron.label) = datum
      state(neuron.label) = datum
    }
  }

  def outputData = out map (n => prevState(n.label))

  def think() {
    for (neuron <- neurons -- in) {
      def sigmoid(x: Float): Float = x / Math.sqrt(x * x + 1).toFloat
      val incoming = graph.incomingEdges(neuron).toList
      val incomingData = incoming.map(edge => prevState(edge.in.label))
      val weights = incoming map weight
      def dot(as: List[Float], bs: List[Float]): Float = ((as zip bs) map { case (a, b) => a * b }).sum

      state(neuron.label) = sigmoid(dot(incomingData, weights) + bias(neuron))
    }

    swapStates()
  }
}
