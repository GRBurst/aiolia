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

case class Graph[+E, +V](edges: Set[Edge], vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {

  // Important for auto ids in grammar expansion. Also important for data indexing in neural network
  assert((0 until vertices.size).forall(vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${vertices}")

  def vertices: Set[Vertex] = edges.flatMap { case Edge(in, out) => Set(in, out) }

  def incomingEdges(v: Vertex) = edges.filter(_.out == v)
  def outgoingEdges(v: Vertex) = edges.filter(_.in == v)
  def predecessors(v: Vertex) = edges.collect { case Edge(i, v) => i }
  def successors(v: Vertex) = edges.collect { case Edge(v, o) => o }
}

