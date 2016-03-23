package aiolia.hypergraphgrammar

object Types {
  type Label = Int
}

import Types._

case class Vertex(label: Label) {
  override def toString = s"($label)"
}

case class Edge(in: Vertex, out: Vertex) {
  override def toString = s"$in -> $out"
}

case class HyperEdge(label: Label, in: List[Vertex] = Nil, out: List[Vertex] = Nil)

case class HyperGraph[+E, +V](hyperEdges: List[HyperEdge] = Nil, edges: Set[Edge] = Set.empty, vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only contain vertices in HyperGraph")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only contain edges in HyperGraph")

  def vertices: Set[Vertex] = {
    val hyperVertices = hyperEdges.flatMap { case HyperEdge(_, in, out) => in ++ out }
    val edgeVertices = edges.flatMap { case Edge(in, out) => Set(in, out) }
    edgeVertices ++ hyperVertices
  }

  def toGraph = {
    assert(hyperEdges.isEmpty)
    Graph[E, V](edges, vertexData, edgeData)
  }
}

case class Graph[+E, +V](edges: Set[Edge], vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {
  assert((0 until vertices.size).forall(vertices contains Vertex(_)), s"${vertices}")

  def vertices: Set[Vertex] = edges.flatMap { case Edge(in, out) => Set(in, out) }

  def incomingEdges(v: Vertex) = edges.filter(_.out == v)
  def outgoingEdges(v: Vertex) = edges.filter(_.in == v)
  def predecessors(v: Vertex) = edges.collect { case Edge(i, v) => i }
  def successors(v: Vertex) = edges.collect { case Edge(v, o) => o }
}

case class MultiPointedHyperGraph[+E, +V](in: List[Vertex] = Nil, out: List[Vertex] = Nil, hyperGraph: HyperGraph[E, V]) {
  assert((in ++ out).forall(vertices contains _), "All tentacles must be used in HyperGraph")

  def vertices = hyperGraph.vertices
  def edges = hyperGraph.edges
  def hyperEdges = hyperGraph.hyperEdges
  def vertexData = hyperGraph.vertexData
  def edgeData = hyperGraph.edgeData
}

case class Grammar[E, V](axiom: HyperGraph[E, V], productions: Map[Label, MultiPointedHyperGraph[E, V]]) {
  //TODO: cycle detection
  def expand = {
    var current = axiom
    val autoId = new AutoId(axiom.vertices.size)
    while (current.hyperEdges.nonEmpty) {
      val lhs = current.hyperEdges.head
      val rhs = productions(lhs.label)

      val newVertices = (rhs.vertices -- (rhs.in ++ rhs.out)).map(_.label -> Vertex(autoId.nextId)).toMap
      val existVertices = rhs.in.map(_.label).zip(lhs.in).toMap ++ rhs.out.map(_.label).zip(lhs.out).toMap
      val vertices: Map[Label, Vertex] = newVertices ++ existVertices

      val edges = rhs.edges.map { case Edge(Vertex(in), Vertex(out)) => Edge(vertices(in), vertices(out)) }

      val hyperEdges = rhs.hyperEdges.map { case HyperEdge(label, in, out) => HyperEdge(label, in.map(v => vertices(v.label)), out.map(v => vertices(v.label))) }

      val vertexData = rhs.vertexData.map { case (Vertex(label), v) => vertices(label) -> v }.toMap
      val edgeData = rhs.edgeData.map { case (Edge(Vertex(in), Vertex(out)), v) => Edge(vertices(in), vertices(out)) -> v }.toMap

      current = HyperGraph(
        (current.hyperEdges ++ hyperEdges) diff List(lhs),
        current.edges ++ edges,
        current.vertexData ++ vertexData,
        current.edgeData ++ edgeData
      )
    }

    current.toGraph
  }
}

class AutoId(var start: Int = 0) {
  def nextId = {
    val current = start
    start += 1
    current
  }
}
