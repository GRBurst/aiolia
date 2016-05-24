package aiolia.neuralNetwork

import aiolia.graph._

object Recurrent {
  def apply(in: List[Vertex], out: List[Vertex], graph: Graph[Double, Double]) = {
    //TODO: cleanup parts of the network which are not reachable from the output vertices
    new Recurrent(in, out, graph)
  }
}

class Recurrent(in: List[Vertex], out: List[Vertex], graph: Graph[Double, Double]) {

  assert(
    (0 until graph.vertices.size).forall(graph.vertices contains Vertex(_)),
    s"vertices need to have labels 0..|vertices|\n${graph.vertices}"
  ) // Important for data indexing

  // assert(in.forall(graph.incomingEdges(_).isEmpty), "Input neurons should not have incoming edges")
  // assert(out.forall(graph.outgoingEdges(_).isEmpty), "Output neurons should not have outgoing edges")

  import graph.{edgeData => weight, vertexData => bias, vertices => neurons, edges => synapses}

  private var currentState = 0
  private val stateArray = Array(Array.fill[Double](graph.vertices.size)(0), Array.fill[Double](graph.vertices.size)(0))
  private def swapStates() { currentState = 1 - currentState }
  def state = stateArray(currentState)
  def prevState = stateArray(1 - currentState)

  def setInputData(data: Iterable[Double]) {
    assert(in.size == data.size, "Need to set all input data")

    for ((neuron, datum) <- in zip data) {
      setInputData(neuron.label, datum)
    }
  }

  def setInputData(index: Int, datum: Double) {
    prevState(index) = datum
    state(index) = datum
  }

  def outputData = out map (n => prevState(n.label))

  def sigmoid(x: Double): Double = x / Math.sqrt(x * x + 1).toDouble
  def dot(as: List[Double], bs: List[Double]): Double = ((as zip bs) map { case (a, b) => a * b }).sum

  def size = neurons.size + synapses.size

  def think() {
    for (neuron <- neurons -- in) {
      val incoming = graph.incomingEdges(neuron).toList
      val incomingData = incoming.map(edge => prevState(edge.in.label))
      val weights = incoming map weight

      //TODO: incomingData and weights come from Sets, so the order may not be correct.
      // only use incomingEdges and get to the neurons from there
      state(neuron.label) = sigmoid(dot(incomingData, weights) + bias(neuron))
    }

    swapStates()
  }
}
