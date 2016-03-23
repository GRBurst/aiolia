package aiolia.neuralnetwork

import aiolia.graph._
import aiolia.hypergraphgrammar._

import collection.mutable

case class NeuralNetwork(in: List[Vertex], out: List[Vertex], graph: Graph[Float, Float]) {
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

  def setInputData(data: List[Float]) {
    for( (neuron, datum) <- (in zip data)) {
      prevState(neuron.label) = datum
      state(neuron.label) = datum
    }
  }

  def outputData = out map(n => prevState(n.label))

  def think() {
    for( neuron <- neurons -- in ) {
      def sigmoid(x: Float):Float = x / Math.sqrt(x*x + 1).toFloat
      val incoming = graph.incomingEdges(neuron).toList
      val incomingData = incoming.map( edge => prevState(edge.in.label) )
      val weights = incoming map weight
      def dot(as:List[Float], bs: List[Float]):Float = ((as zip bs) map {case (a, b) => a*b}).sum

      state(neuron.label) = sigmoid(dot(incomingData, weights) + bias(neuron))
    }

    swapStates()
  }
}
