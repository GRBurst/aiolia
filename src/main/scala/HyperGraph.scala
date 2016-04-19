package aiolia.hypergraph

import aiolia.graph._
import aiolia.graph.types._

case class HyperEdge(label: Label, connectors: List[Vertex] = Nil) {
  def contains(v: Vertex) = connectors contains v
  override def toString = s"[$label:${connectors.mkString("-")}]"
  //TODO: should connectors be distinct?
}

//TODO: directed traversal needs in/out, but right now, there is no directed traversal
//TODO: order on List[HyperEdge] should not matter, especially on comparison. We probably need a "Bag" datastructure: https://github.com/nicolasstucki/multisets

case class HyperGraph[+V, +E](vertices: Set[Vertex] = Set.empty, hyperEdges: List[HyperEdge] = Nil, edges: Set[Edge] = Set.empty, vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {
  assert(hyperEdges.flatMap(_.connectors).forall(vertices contains _), "All vertices used in hyperedges have to be defined")
  assert(edges.flatMap(e => Seq(e.in, e.out)).forall(vertices contains _), "All vertices used in edges have to be defined")
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only contain vertices in HyperGraph")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only contain edges in HyperGraph")

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

  def -(v:Vertex) = {
    assert(vertices contains v, s"Vertex $v does not exist in ${vertices}")

    val (removedEdges, retainedEdges) = edges.partition(_ contains v)

    HyperGraph(
      vertices = vertices - v,
      hyperEdges = hyperEdges.filterNot(_.connectors contains v),
      edges = retainedEdges,
      vertexData = vertexData - v,
      edgeData = edgeData -- removedEdges
    )
  }

  def -(e:Edge) = {
    assert(edges contains e, s"Edge $e does not exist in ${edges}")

    copy( edges = edges - e, edgeData = edgeData - e )
  }

  def -(h:HyperEdge) = {
    assert(hyperEdges contains h, s"HyperEdge $h does not exist in ${hyperEdges}")

    val i = hyperEdges indexOf h
    copy( hyperEdges = hyperEdges.take(i) ++ hyperEdges.drop(i+1) )
  }

  def --[E1,V1](removed:HyperGraph[E1,V1]) = {
    assert(removed.vertices.forall(vertices contains _))
    assert(removed.edges.forall(edges contains _))
    assert(removed.hyperEdges.forall(hyperEdges contains _))

    val implicitlyRemovedEdges = edges.filter(e => (removed.vertices contains e.in) || (removed.vertices contains e.out))
    val removedEdges = removed.edges ++ implicitlyRemovedEdges

    HyperGraph(
      vertices -- removed.vertices,
      hyperEdges diff removed.hyperEdges,//TODO: implicitly removed hyperEdges
      edges -- removedEdges,
      vertexData -- removed.vertexData.keys -- vertices, //TODO: is "-- vertices" needed?
      edgeData -- removed.edgeData.keys -- removedEdges
    )
  }

  def neighbours(v: Vertex):Set[Vertex] = edges.collect {
    //TODO: over hyperedges?
    case Edge(`v`, out) => out
    case Edge(in, `v`) => in
  }
  def neighbours(vs:Iterable[Vertex]):Set[Vertex] = vs.flatMap(neighbours).toSet -- vs

  def incidentEdges(v: Vertex):Set[Edge] = edges.filter(_ contains v)
  def incidentEdges(vs:Iterable[Vertex]):Set[Edge] = vs.flatMap(incidentEdges).toSet
  def incidentHyperEdges(v: Vertex):List[HyperEdge] = hyperEdges.filter(_ contains v)
  def incidentHyperEdges(vs:Iterable[Vertex]):List[HyperEdge] = {
    val vsSet = vs.toSet
    hyperEdges.filter(_.connectors.exists(vsSet contains _))
  }

  def inducedEdges(vs:Iterable[Vertex]):Set[Edge] = {
    val vsSet = vs.toSet
    edges.filter(e => (vsSet contains e.in) && (vsSet contains e.out))
  }

  def inducedHyperEdges(vs:Iterable[Vertex]):List[HyperEdge] = {
    val vsSet = vs.toSet
    hyperEdges.filter(_.connectors.forall(vsSet contains _))
  }

  def inducedSubGraph(vs:Iterable[Vertex]):HyperGraph[V,E] = {
    val vsSet = vs.toSet
    HyperGraph(vsSet, inducedHyperEdges(vs), inducedEdges(vs))
  }

  def toGraph = {
    assert(hyperEdges.isEmpty)
    Graph[V, E](vertices, edges, vertexData, edgeData)
  }
}
