package aiolia.hypergraph

import aiolia.graph._
import aiolia.graph.types._

case class HyperEdge(label: Label, in: List[Vertex] = Nil, out: List[Vertex] = Nil)

case class HyperGraph[+E, +V](vertices: Set[Vertex] = Set.empty, hyperEdges: List[HyperEdge] = Nil, edges: Set[Edge] = Set.empty, vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only contain vertices in HyperGraph")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only contain edges in HyperGraph")
  assert(edges.flatMap(e => Seq(e.in, e.out)).forall(vertices contains _), "All vertices used in edges have to be defined")
  assert(hyperEdges.flatMap(e => e.in ++ e.out).forall(vertices contains _), "All vertices used in hyperedges have to be defined")

  def toGraph = {
    assert(hyperEdges.isEmpty)
    Graph[E, V](vertices, edges, vertexData, edgeData)
  }
}
