package aiolia.circuit

import aiolia.graph._
import aiolia.util.{Compiler, AutoId}

final case class Circuit(in: List[Vertex], out: List[Vertex], graph: Graph[Nothing, Nothing]) {
  // Important for data indexing in neural network
  assert((0 until graph.vertices.size).forall(graph.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${graph.vertices.toSeq.sortBy(_.label)}")

  assert(in.forall(graph.incomingEdges(_).isEmpty), "Input vertices can not have incoming edges")
  assert(out.forall(graph.outgoingEdges(_).isEmpty), "Output vertices can not have outgoing edges")
  assert(out.forall(graph.inDegree(_) <= 1), "Output vertices are not gates, so they can only have one input")

  assert(!graph.hasCycle, "Graph must not have cycles")

  import graph.{vertices => gates, edges => wires}

  lazy val connectors = in ++ out

  private lazy val computeInputs: Array[Label] = in.map(_.label).toArray
  private lazy val computeOutputs: Array[Label] = out.map(_.label).toArray
  private lazy val reachableFromOut: List[Vertex] = (out flatMap { v => graph.depthFirstSearch(v, graph.predecessors) }).distinct
  private lazy val computeOrder: Array[Label] = ((graph.topologicalSort intersect reachableFromOut) diff in diff out).map(_.label).toArray
  private lazy val computeIncomingWires: Array[Array[Label]] = {
    val a = new Array[Array[Label]](gates.size)
    for (g <- computeOrder) {
      a(g) = graph.incomingEdges(Vertex(g)).map { case Edge(pre, _) => pre.label }.toArray
    }
    for (g <- computeOutputs) {
      a(g) = graph.incomingEdges(Vertex(g)).map { case Edge(pre, _) => pre.label }.toArray
    }
    a
  }

  def computeIntermediateResults(data: Array[Boolean]): Array[Boolean] = {
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

    for (n <- computeOutputs) {
      var tmp = false
      for (pre <- computeIncomingWires(n)) {
        tmp = results(pre)
      }
      results(n) = tmp
    }
    results
  }

  private var compute_compiled: Option[(IndexedSeq[Boolean]) => Array[Boolean]] = None
  def compile() {
    val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
    import universe._

    val nodes = (graph.topologicalSort intersect (out flatMap { v => graph.depthFirstSearch(v, graph.predecessors) }).distinct) diff out
    val node_code = nodes map { n =>
      val outData = in.indexOf(n) match {
        case i if i >= 0 => q"data($i)"
        case -1 =>
          graph.incomingEdges(n).toList match {
            case Nil => q"false"
            case es =>
              val ins: List[Tree] = (es map { case e @ Edge(pre, _) => q"${TermName(s"v${pre.label}")}" })
              val andIn = ins.reduce((a, b) => q"$a && $b")
              q"!$andIn"
          }
      }
      q"val ${TermName(s"v${n.label}")}:Boolean = $outData"
    }

    val out_node_code = out map { n =>
      val outData = in.indexOf(n) match {
        case i if i >= 0 => q"data($i)"
        case -1 =>
          graph.incomingEdges(n).toList match {
            case Nil => q"false"
            case es =>
              val ins: List[Tree] = (es map { case e @ Edge(pre, _) => q"${TermName(s"v${pre.label}")}" })
              q"${ins.head}"
          }
      }
      q"val ${TermName(s"v${n.label}")}:Boolean = $outData"
    }

    val result = out map { v => q"${TermName(s"v${v.label}")}" }

    val code = q"(data:IndexedSeq[Boolean]) => {..$node_code;..$out_node_code;Array[Boolean](..$result)}"
    // println(showCode(code))

    compute_compiled = Some(Compiler[(IndexedSeq[Boolean]) => Array[Boolean]](code))
  }

  def compute(data: Array[Boolean]): Array[Boolean] = {
    compute_compiled.foreach { computeFunc => return computeFunc(data) }
    val results = computeIntermediateResults(data)

    // return Array of results of output vertices
    computeOutputs map results
  }

  def insertSubCircuit(inputs: Seq[Vertex], outputs: Seq[Vertex], subCircuit: Circuit): Circuit = {
    assert(inputs forall this.graph.vertices)
    assert(outputs forall this.graph.vertices)
    assert(inputs.size + outputs.size == subCircuit.connectors.size)
    val autoId = AutoId(this.graph.vertices.map(_.label).max + 1)

    val newGraph = this.graph.insertSubGraph(inputs ++ outputs, subCircuit.connectors, subCircuit.graph, autoId)
    Circuit(this.in, this.out, newGraph)
  }
}
