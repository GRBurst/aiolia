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
case class HyperEdge(label: Label, in:List[Vertex], out:List[Vertex])

case class HyperGraphTemplate(hyperEdges: Set[HyperEdgeTemplate] = Set.empty, edges: Set[EdgeTemplate] = Set.empty, in: List[Label] = Nil, out:List[Label] = Nil) {
  def vertices:Set[Label] = {
    val hyperVertices = hyperEdges.flatMap { case HyperEdgeTemplate(_, in, out) => in ++ out }
    val edgeVertices = edges.flatMap { case EdgeTemplate(in, out) => Set(in, out) }
    hyperVertices ++ edgeVertices
  }
}
case class HyperEdgeTemplate(label:Label, in: List[Label], out:List[Label])
case class EdgeTemplate(in:Label, out:Label)

case class Graph(edges: Set[Edge]) {
  def vertices:Set[Vertex] = edges.flatMap { case Edge(in, out) => Set(in, out) }
  assert( (0 until vertices.size).forall(vertices contains Vertex(_)), s"${vertices}" )
}
case class Edge(in:Vertex, out:Vertex) {
  override def toString = s"$in -> $out"
}

case class Grammar(axiom: HyperGraph, productions: Map[Label, HyperGraphTemplate]) {
  def expand = {
    var current = axiom
    val autoId = new AutoId(axiom.vertices.size)
    while( current.hyperEdges.nonEmpty ) {
      val nonTerminal = current.hyperEdges.head
      val replacement = productions(nonTerminal.label)

      val newVertices = (replacement.vertices -- (replacement.in ++ replacement.out)).map(label => label -> Vertex(autoId.nextId)).toMap
      val existVertices = replacement.in.zip(nonTerminal.in).toMap ++ replacement.out.zip(nonTerminal.out).toMap
      val vertices = newVertices ++ existVertices

      val edges = replacement.edges.map{ case EdgeTemplate(in, out) => Edge(vertices(in), vertices(out)) }

      current = HyperGraph(
        current.hyperEdges - nonTerminal,
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

