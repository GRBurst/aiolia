package aiolia

import aiolia.graph._

import collection.mutable

object FeedForwardNeuralNetwork {
  def apply(in: List[Vertex], out: List[Vertex], graph: Graph[Double, Double]) = {
    // val badEdges = graph.edges.filter{ case Edge(i, o) => (in contains o) || (out contains i) }
    // val cleanedGraph = graph.copy(edges = graph.edges -- badEdges, edgeData = graph.edgeData -- badEdges)
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

  private var compute_compiled: Option[Function1[IndexedSeq[Double], Array[Double]]] = None
  def compile() {
    val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
    import universe._

    val nodes = graph.topologicalSort intersect (out flatMap { v => graph.depthFirstSearch(v, graph.predecessors) }).distinct
    val node_code = nodes map { n =>
      val nbias = bias.get(n).getOrElse(0.0)
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

    val sigmoid = q"final def sigmoid(x: Double): Double = x / Math.sqrt(x * x + 1) " //TODO: inline
    val code = q"(data:IndexedSeq[Double]) => {$sigmoid;..$node_code;Array[Double](..$result)}"
    // println(showCode(code))

    compute_compiled = Some(Compiler[Function1[IndexedSeq[Double], Array[Double]]](code))
  }

  def sigmoid(x: Double): Double = x / Math.sqrt(x * x + 1)
  def compute(data: IndexedSeq[Double]): Array[Double] = {
    compute_compiled.foreach { compute => return compute(data) }

    val cachedResults = mutable.HashMap[Vertex, Double]()
    // println(s"compute: on $graph\nin: $in -> out:$out\ndata: $data")
    def eval(neuron: Vertex): Double = {
      cachedResults.get(neuron).foreach{ return _ }

      val outData = in.indexOf(neuron) match {
        case i if i >= 0 => data(i) //TODO: send input data through sigmoid?
        case -1 =>
          val inputs = graph.incomingEdges(neuron).toList
          inputs match {
            case Nil => 0 //TODO: nodes without inputs = sigmoid(bias)?
            case es =>
              (es map { case e @ Edge(in, _) => eval(in) * weight(e) }).sum
          }
      }
      val result = sigmoid(outData + bias.get(neuron).getOrElse(0.0)) //TODO: assert that all neurons have bias?
      cachedResults(neuron) = result
      result
    }

    (out map eval).toArray
  }
}
