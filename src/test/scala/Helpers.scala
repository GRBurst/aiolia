package aiolia.test

import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

object Helpers {
  implicit def vertexTupleToEdge(tuple: (Int, Int)) = Edge(Vertex(tuple._1), Vertex(tuple._2))
  implicit def intToVertex(i: Int) = Vertex(i)
  def vertexSet(labels: Int*) = labels.map(Vertex(_)).toSet
  def edgeSet(edges: (Int, Int)*) = edges.map{case (in, out) => Edge(Vertex(in), Vertex(out))}.toSet
  implicit def intToVertexSet(i: Int):Set[Vertex] = (0 to i).map(Vertex(_)).toSet
  implicit def intToAxiom(i: Int) = HyperGraph(hyperEdges = List(HyperEdge(i)))
  def vertexData[V](data: (Int, V)*) = data.map { case (label, data) => Vertex(label) -> data }.toMap
  def edgeData[E](data: ((Int, Int), E)*) = data.map { case ((a, b), data) => Edge(a, b) -> data }.toMap
}
