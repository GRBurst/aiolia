package aiolia.graph

package object types {
  type Label = Int
}

import types._

case class Vertex(label: Label) {
  override def toString = s"($label)"
}

case class Edge(in: Vertex, out: Vertex) {
  override def toString = s"$in -> $out"
}

case class Graph[+E, +V](vertices: Set[Vertex], edges: Set[Edge], vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {

  assert(edges.flatMap(e => Seq(e.in, e.out)).forall(vertices contains _), "All vertices used in edges have to be defined")

  def incomingEdges(v: Vertex) = edges.filter(_.out == v)
  def outgoingEdges(v: Vertex) = edges.filter(_.in == v)
  def predecessors(v: Vertex) = edges.collect { case Edge(i, `v`) => i }
  def successors(v: Vertex) = edges.collect { case Edge(`v`, o) => o }

  def hasCycle = {
    //TODO: optimize for seen vertices
    def visit(v: Vertex, visited: Set[Vertex] = Set.empty): Boolean = {
      if(visited.contains(v))
        false
      else
        successors(v).forall(visit(_ , visited + v))
    }

    vertices.exists(!visit(_))
  }

  override def toString = s"Graph(0 to ${vertices.size - 1}, $edges, $vertexData, $edgeData)"
}

