package aiolia.test

import aiolia._
import aiolia.graph._
import aiolia.graph.types._

// TODO: test helpers
// TODO: move dsl to package graph.dsl

object Helpers {
  // Graph construction
  def v(label: Label) = Vertex(label)
  def V(labels: Range) = labels.map(Vertex(_)).toSet
  def V(labels: Label*) = labels.map(Vertex(_)).toSet
  def VL(labels: Label*) = labels.map(Vertex(_)).toList
  def e(edge: (Label, Label)) = edge match { case (in, out) => Edge(Vertex(in), Vertex(out)) }
  def E(es: (Label, Label)*) = es.map{ case (in, out) => Edge(Vertex(in), Vertex(out)) }.toSet
  def C(labels: Label*) = labels.map(Vertex(_)).toList // connectors
  //TODO: vertexData, edgeData shorter?
  def vertexData[V](data: (Int, V)*) = data.map { case (label, data) => Vertex(label) -> data }.toMap
  def edgeData[E](data: ((Int, Int), E)*) = data.map { case ((a, b), data) => e(a -> b) -> data }.toMap

  // default parameter order
  def graph[V, E](
    v:   Set[Vertex],
    e:   Set[Edge]         = Set.empty,
    vd:  Map[Vertex, V]    = Map.empty[Vertex, V],
    ed:  Map[Edge, E]      = Map.empty[Edge, E],
    nts: List[NonTerminal] = Nil,
    c:   List[Vertex]      = Nil
  ) = Graph(v, e, vd, ed, nts, c)

  // production: connectors first and mandatory
  def cgraph[V, E](
    c:   List[Vertex]      = Nil,
    v:   Set[Vertex]       = Set.empty,
    e:   Set[Edge]         = Set.empty,
    vd:  Map[Vertex, V]    = Map.empty[Vertex, V],
    ed:  Map[Edge, E]      = Map.empty[Edge, E],
    nts: List[NonTerminal] = Nil
  ) = Graph(vertices = v, edges = e, vertexData = vd, edgeData = ed, nonTerminals = nts, connectors = c)

  def cgraph[V, E](
    nt:  NonTerminal,
    nts: NonTerminal*
  ) = Graph(nonTerminals = nt :: nts.toList)

  def graph[V, E](
    nt:  NonTerminal,
    nts: NonTerminal*
  ) = Graph(nonTerminals = nt :: nts.toList)

  def graph[V, E]() = Graph.empty

  // NonTerminal
  def NT(l: Label, c: Product = None) = { // TODO: lowercase, because single element
    assert(!c.isInstanceOf[List[_]], s"don't put Lists in NT($l, $c). Use a Tuple instead: NT($l, ${c.asInstanceOf[List[_]].mkString("(", ",", ")")})")
    NonTerminal(l, c.productIterator.toList.asInstanceOf[List[Int]].map(Vertex(_)))
  }

  def A(label: Label) = Graph(nonTerminals = List(NonTerminal(label)))

  def grammar[V, E](axiom: Graph[V, E], rules: (Label, Graph[V, E])*): Grammar[V, E] = {
    Grammar(axiom, rules.map{ case (label, graph) => label -> graph }.toMap)
  }
}
