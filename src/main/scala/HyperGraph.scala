package aiolia.hypergraph

import aiolia.graph._
import aiolia.graph.types._

case class HyperEdge(label: Label, connectors: List[Vertex] = Nil)

case class HyperGraph[+E, +V](vertices: Set[Vertex] = Set.empty, hyperEdges: List[HyperEdge] = Nil, edges: Set[Edge] = Set.empty, vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only contain vertices in HyperGraph")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only contain edges in HyperGraph")
  assert(edges.flatMap(e => Seq(e.in, e.out)).forall(vertices contains _), "All vertices used in edges have to be defined")
  assert(hyperEdges.flatMap(_.connectors).forall(vertices contains _), "All vertices used in hyperedges have to be defined")

  def -(v:Vertex) = {
    assert(vertices contains v, s"Vertex $v does not exist in ${vertices}")

    copy(
      vertices = vertices - v,
      hyperEdges = hyperEdges.filterNot(_.connectors contains v),
      edges = edges.filterNot(e => e.in == v || e.out == v)
    )
  }

  def -(e:Edge) = {
    assert(edges contains e, s"Edge $e does not exist in ${edges}")

    copy( edges = edges - e )
  }

  def -(h:HyperEdge) = {
    assert(hyperEdges contains h, s"HyperEdge $h does not exist in ${hyperEdges}")

    val i = hyperEdges indexOf h
    copy( hyperEdges = hyperEdges.take(i) ++ hyperEdges.drop(i+1) )
  }

  def +(v:Vertex) = {
    assert(!(vertices contains v), s"Vertex $v already exists in ${vertices}")

    copy(vertices = vertices + v)
  }

  def +(e:Edge) = {
    assert(!(edges contains e), s"Edge $e already exists in ${edges}")

    copy(edges = edges + e)
  }

  def +(h:HyperEdge) = {
    copy(hyperEdges = h :: hyperEdges)
  }

  def toGraph = {
    assert(hyperEdges.isEmpty)
    Graph[E, V](vertices, edges, vertexData, edgeData)
  }
}
