package aiolia.hypergraphgrammar

import aiolia.helpers._
import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

case class MultiPointedHyperGraph[+E, +V](in: List[Vertex] = Nil, out: List[Vertex] = Nil, hyperGraph: HyperGraph[E, V] = HyperGraph()) {
  assert((in ++ out).forall(vertices contains _), "All tentacles must be used in HyperGraph")

  def vertices = hyperGraph.vertices
  def edges = hyperGraph.edges
  def hyperEdges = hyperGraph.hyperEdges
  def vertexData = hyperGraph.vertexData
  def edgeData = hyperGraph.edgeData

  def -(v:Vertex) = {
    assert(!(in contains v), s"Cannot remove input vertex $v")
    assert(!(out contains v), s"Cannot remove output vertex $v")

    copy( hyperGraph = hyperGraph - v )
  }

  def -(e:Edge) = {
    copy( hyperGraph = hyperGraph - e )
  }

  def -(h:HyperEdge) = {
    copy( hyperGraph = hyperGraph - h )
  }

  def +(v:Vertex) = ???

  def +(e:Edge) = ???

  def +(h:HyperEdge) = ???
}

case class Grammar[E, V](axiom: HyperGraph[E, V], productions: Map[Label, MultiPointedHyperGraph[E, V]] = Map.empty[Label, MultiPointedHyperGraph[E, V]]) {
  assert((0 until axiom.vertices.size).forall(axiom.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${axiom.vertices}")
  assert(productions.values.flatMap(_.hyperEdges).forall { hyperEdge =>
    val rhs = productions.get(hyperEdge.label)
    rhs.isDefined && (hyperEdge.in.size == rhs.get.in.size) && (hyperEdge.out.size == rhs.get.out.size)
  }, "All hyperedges on the rhs need to have an equivalent on the lhs")
  //TODO: assert: no cycles in grammar allowed

  def +(production: (Label,MultiPointedHyperGraph[E,V])) = ???
  def -(hyperEdge: Label) = ???

  def expand = {
    // TODO: make stateless - with a fold?
    var current = axiom
    val autoId = new AutoId(axiom.vertices.size)
    while (current.hyperEdges.nonEmpty) {
      val lhs = current.hyperEdges.head
      val rhs = productions(lhs.label)

      // newly created vertices that will be merged into the graph at fringe vertices
      val newVertices = (rhs.vertices -- (rhs.in ++ rhs.out)).map(_.label -> Vertex(autoId.nextId)).toMap
      // existing fringe/connectivity vertices for merge process
      val existVertices = rhs.in.map(_.label).zip(lhs.in).toMap ++ rhs.out.map(_.label).zip(lhs.out).toMap
      val vertexMap: Map[Label, Vertex] = newVertices ++ existVertices

      val vertices = rhs.vertices.map(v => vertexMap(v.label))

      val edges = rhs.edges.map { case Edge(Vertex(in), Vertex(out)) => Edge(vertexMap(in), vertexMap(out)) }

      val hyperEdges = rhs.hyperEdges.map { case HyperEdge(label, in, out) => HyperEdge(label, in.map(v => vertexMap(v.label)), out.map(v => vertexMap(v.label))) }

      val vertexData = rhs.vertexData.map { case (Vertex(label), v) => vertexMap(label) -> v }.toMap
      val edgeData = rhs.edgeData.map { case (Edge(Vertex(in), Vertex(out)), v) => Edge(vertexMap(in), vertexMap(out)) -> v }.toMap

      current = HyperGraph(
        current.vertices ++ vertices,
        (current.hyperEdges ++ hyperEdges) diff List(lhs),
        current.edges ++ edges,
        current.vertexData ++ vertexData,
        current.edgeData ++ edgeData
      )
    }

    current.toGraph
  }
}

