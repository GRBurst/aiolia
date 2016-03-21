package aiolia.hypergraphgrammar

object Types {
  type Label = Int
}

import Types._

case class HyperGraph(hyperEdges: Set[HyperEdge] = Set.empty, edges: Set[Edge] = Set.empty) {
  def vertices:Set[Vertex] = {
    val hyperVertices = hyperEdges.flatMap { case HyperEdge(_, in, out) => in ++ out }
    val edgeVertices = edges.flatMap { case Edge(in, out) => Set(in, out) }
    hyperVertices ++ edgeVertices
  }

  def toGraph = {
    assert(hyperEdges.isEmpty)
    Graph(edges)
  }
}

case class MultiPointedHyperGraph(in: List[Vertex], out:List[Vertex], hyperGraph: HyperGraph) {
  def vertices = hyperGraph.vertices
  def edges = hyperGraph.edges
  def hyperEdges = hyperGraph.hyperEdges
}

object MultiPointedHyperGraph {
  def apply(in: List[Vertex] = Nil, out:List[Vertex] = Nil, hyperEdges: Set[HyperEdge] = Set.empty, edges: Set[Edge] = Set.empty):MultiPointedHyperGraph = {
    MultiPointedHyperGraph(in, out, HyperGraph(hyperEdges, edges))
  }
}

case class HyperEdge(label: Label, in:List[Vertex], out:List[Vertex])

case class Graph(edges: Set[Edge]) {
  def vertices:Set[Vertex] = edges.flatMap { case Edge(in, out) => Set(in, out) }
  assert( (0 until vertices.size).forall(vertices contains Vertex(_)), s"${vertices}" )
}

case class Edge(in:Vertex, out:Vertex) {
  override def toString = s"$in -> $out"
}

case class Grammar(axiom: HyperGraph, productions: Map[Label, MultiPointedHyperGraph]) {
  //TODO: cycle detection
  def expand = {
    var current = axiom
    val autoId = new AutoId(axiom.vertices.size)
    while( current.hyperEdges.nonEmpty ) {
      val lhs = current.hyperEdges.head
      val rhs = productions(lhs.label)

      val newVertices = (rhs.vertices -- (rhs.in ++ rhs.out)).map(_.label -> Vertex(autoId.nextId)).toMap
      val existVertices = rhs.in.map(_.label).zip(lhs.in).toMap ++ rhs.out.map(_.label).zip(lhs.out).toMap
      val vertices:Map[Label, Vertex] = newVertices ++ existVertices

      val edges = rhs.edges.map{ case Edge(Vertex(in), Vertex(out)) => Edge(vertices(in), vertices(out)) }

      val hyperEdges = rhs.hyperEdges.map{ case HyperEdge(label, in, out) => HyperEdge(label, in.map(v => vertices(v.label)), out.map(v => vertices(v.label))) }

      current = HyperGraph(
        current.hyperEdges ++ hyperEdges - lhs,
        current.edges ++ edges)
    }

    current.toGraph
  }
}
case class Vertex(label: Label) {
  override def toString = s"($label)"
}

class AutoId(var start: Int = 0) {
  def nextId = {
    val current = start
    start += 1
    current
  }
}

