package aiolia.neuralNetwork

import aiolia.graph._
import aiolia.util.Compiler

object FeedForward {
  def apply(in: List[Vertex], out: List[Vertex], graph: Graph[Double, Double]) = {
    new FeedForward(in, out, graph)
  }
}

//TODO: sigmoid: x / Math.sqrt(1 + x*x) ? // which is 1.75 times faster than tanh

class FeedForward private (in: List[Vertex], out: List[Vertex], graph: Graph[Double, Double]) {
  // Important for data indexing in neural network
  assert((0 until graph.vertices.size).forall(graph.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${graph.vertices}")

  assert(in.forall(graph.incomingEdges(_).isEmpty), "Input neurons can not have incoming edges")
  assert(out.forall(graph.outgoingEdges(_).isEmpty), "Output neurons can not have outgoing edges")
  assert(graph.vertices.forall(v => util.Try(graph.vertexData(v)).isSuccess), "All neurons need to have a bias")
  assert(graph.edges.forall(e => util.Try(graph.edgeData(e)).isSuccess), "All synapses need to have a weight")

  assert(!graph.hasCycle)

  import graph.{edgeData => weight, vertexData => bias, vertices => neurons}

  private var compute_compiled: Option[(IndexedSeq[Double]) => Array[Double]] = None
  def compile() {
    val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
    import universe._

    val nodes = graph.topologicalSort intersect (out flatMap { v => graph.depthFirstSearch(v, graph.predecessors) }).distinct
    val node_code = nodes map { n =>
      val nbias = bias(n)
      val outData = in.indexOf(n) match {
        case i if i >= 0 => q"data($i) + $nbias"
        case -1 =>
          graph.incomingEdges(n).toList match {
            case Nil => q"$nbias"
            case es =>
              val dotProduct = (es map { case e @ Edge(pre, _) => q"${TermName(s"v${pre.label}")} * ${weight(e)}" }).reduce((a, b) => q"$a + $b")
              q"$dotProduct + $nbias"
          }
      }
      q"val ${TermName(s"v${n.label}")}:Double = sigmoid($outData)"
    }

    val result = out map { v => q"${TermName(s"v${v.label}")}" }

    val sigmoid = q"final def sigmoid(x: Double): Double = Math.tanh(x) " //TODO: inline
    val code = q"(data:IndexedSeq[Double]) => {$sigmoid;..$node_code;Array[Double](..$result)}"
    // println(showCode(code))

    compute_compiled = Some(Compiler[(IndexedSeq[Double]) => Array[Double]](code))
  }

  private val computeOrder: Array[Label] = ((graph.topologicalSort intersect (out flatMap { v => graph.depthFirstSearch(v, graph.predecessors) }).distinct) diff in).map(_.label).toArray
  private val computeInputs: Array[Label] = in.map(_.label).toArray
  private val computeOutputs: Array[Label] = out.map(_.label).toArray
  private val computeBias: Array[Double] = {
    val a = new Array[Double](neurons.size)
    bias.foreach{ case (v, b) => a(v.label) = b }
    a
  }
  private val computeIncomingEdges: Array[Array[(Double, Label)]] = {
    val a = new Array[Array[(Double, Label)]](neurons.size)
    for (n <- computeOrder) {
      a(n) = graph.incomingEdges(Vertex(n)).map{ case e @ Edge(pre, _) => (weight(e), pre.label) }.toArray
    }
    a
  }
  def sigmoid(x: Double): Double = Math.tanh(x)
  def compute(data: Array[Double]): Array[Double] = {
    compute_compiled.foreach { computeFunc => return computeFunc(data) }

    val results = new Array[Double](neurons.size)

    // insert data
    var i = 0
    for (input <- computeInputs) {
      results(input) = sigmoid(data(i) + computeBias(input))
      i += 1
    }

    // compute value for each neuron in topological order
    for (n <- computeOrder) {
      var tmp = 0.0
      for ((weight, pre) <- computeIncomingEdges(n)) {
        tmp += weight * results(pre)
      }
      results(n) = sigmoid(tmp + computeBias(n))
    }

    // return Array of results of output vertices
    computeOutputs map results
  }
}
