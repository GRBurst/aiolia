package aiolia.hypergraphgrammar

import aiolia.helpers._
import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

case class MultiPointedHyperGraph[+E, +V](in: List[Vertex] = Nil, out: List[Vertex] = Nil, hyperGraph: HyperGraph[E, V]) {
  assert((in ++ out).forall(vertices contains _), "All tentacles must be used in HyperGraph")

  def vertices = hyperGraph.vertices
  def edges = hyperGraph.edges
  def hyperEdges = hyperGraph.hyperEdges
  def vertexData = hyperGraph.vertexData
  def edgeData = hyperGraph.edgeData
}

case class Grammar[E, V](axiom: HyperGraph[E, V], productions: Map[Label, MultiPointedHyperGraph[E, V]]) {
  assert(productions.values.flatMap(_.hyperEdges).forall { hyperEdge =>
    val rhs = productions.get(hyperEdge.label)
    rhs.isDefined && (hyperEdge.in.size == rhs.get.in.size) && (hyperEdge.out.size == rhs.get.out.size)
  }, "All hyperedges on the rhs need to have an equivalent on the lhs")

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

