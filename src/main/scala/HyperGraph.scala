package aiolia.hypergraph

import aiolia.graph._
import aiolia.graph.types._

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
