package aiolia.neuralNetwork

import aiolia.graph._

object Circuit {
  def apply(in: List[Vertex], out: List[Vertex], graph: Graph[Nothing, Nothing]) = {
    new Circuit(in, out, graph)
  }
}

class Circuit private (in: List[Vertex], out: List[Vertex], graph: Graph[Nothing, Nothing]) {
  // Important for data indexing in neural network
  assert((0 until graph.vertices.size).forall(graph.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${graph.vertices}")

  assert(in.forall(graph.incomingEdges(_).isEmpty), "Input neurons can not have incoming edges")
  assert(out.forall(graph.outgoingEdges(_).isEmpty), "Output neurons can not have outgoing edges")

  assert(!graph.hasCycle)

  assert(
    (graph.vertices -- in).forall(v => graph.inDegree(v) <= 2),
    "Every result node needs to have max 2 inputs"
  )

  import graph.{vertices => gates, edges => wires}

  private val computeInputs: Array[Label] = in.map(_.label).toArray
  private val computeOutputs: Array[Label] = out.map(_.label).toArray
  private val reachableFromOut: List[Vertex] = (out flatMap { v => graph.depthFirstSearch(v, graph.predecessors) }).distinct
  private val computeOrder: Array[Label] = ((graph.topologicalSort intersect reachableFromOut) diff in).map(_.label).toArray
  private val computeIncomingWires: Array[Array[Label]] = {
    val a = new Array[Array[Label]](gates.size)
    for (g <- computeOrder) {
      a(g) = graph.incomingEdges(Vertex(g)).map { case Edge(pre, _) => pre.label }.toArray
      assert(a(g).size <= 2)
    }
    a
  }

  def compute(data: Array[Boolean]): Array[Boolean] = {
    // intermadiate results
    val results = new Array[Boolean](gates.size)

    // insert data
    var i = 0
    for (input <- computeInputs) {
      results(input) = data(i)
      i += 1
    }

    // compute NAND value for each gate in topological order
    for (n <- computeOrder) {
      var tmp = true
      for (pre <- computeIncomingWires(n)) {
        tmp &&= results(pre)
      }
      results(n) = !tmp
    }

    // return Array of results of output vertices
    computeOutputs map results
  }
}
