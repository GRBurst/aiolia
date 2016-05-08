package aiolia

import aiolia.graph._

import collection.mutable

object FeedForwardNeuralNetwork {
  def apply(in: List[Vertex], out: List[Vertex], graph: Graph[Double, Double]) = {
    new FeedForwardNeuralNetwork(in, out, graph)
  }
}

class FeedForwardNeuralNetwork(in: List[Vertex], out: List[Vertex], graph: Graph[Double, Double]) {
  // Important for data indexing in neural network
  assert((0 until graph.vertices.size).forall(graph.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${graph.vertices}")

  assert(in.forall(graph.incomingEdges(_).isEmpty), "Input neurons can not have incoming edges")
  assert(out.forall(graph.outgoingEdges(_).isEmpty), "Output neurons can not have outgoing edges")

  assert(!graph.hasCycle)

  import graph.{vertices => neurons}
  import graph.{vertexData => bias}
  import graph.{edgeData => weight}

  def sigmoid(x: Double): Double = x / Math.sqrt(x * x + 1).toFloat
  def compute(data: List[Double]): List[Double] = {
    //TODO: optimization: cache already calculated neurons in hashmap
    def eval(neuron: Vertex): Double = {
      val outData = in.indexOf(neuron) match {
        case i if i >= 0 => data(i) //TODO: send input data through sigmoid?
        case -1 =>
          val inputs = graph.incomingEdges(neuron).toList
          inputs match {
            case Nil => bias(neuron) //TODO: nodes without inputs = sigmoid(bias)?
            case es =>
              (es map { case e @ Edge(in, _) => eval(in) * weight(e) }).sum
          }
      }
      sigmoid(outData + bias(neuron))
    }

    out map eval
  }
}
