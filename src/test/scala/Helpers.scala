package aiolia.test

import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

object Helpers {
  implicit def VertexTupleToEdge(tuple: (Int, Int)) = Edge(Vertex(tuple._1), Vertex(tuple._2))
  implicit def IntToVertex(i: Int) = Vertex(i)
  implicit def IntToVertexSet(i: Int):Set[Vertex] = (0 to i).map(Vertex(_)).toSet
  implicit def IntToAxiom(i: Int) = HyperGraph(hyperEdges = List(HyperEdge(i)))
  def vertexData[V](data: (Int, V)*) = data.map { case (label, data) => Vertex(label) -> data }.toMap
  def edgeData[E](data: ((Int, Int), E)*) = data.map { case ((a, b), data) => Edge(a, b) -> data }.toMap
}
