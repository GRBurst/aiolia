package aiolia.test

import aiolia.grammar.Grammar
import aiolia.graph._
import org.mockito.ArgumentMatcher
import org.mockito.Matchers.argThat

object Helpers {
  //TODO: move graph helper functions to dsl?
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

  def A(label: Label) = Graph(nonTerminals = List(NonTerminal(label)))

  def grammar[V, E](axiom: Graph[V, E], rules: (Label, Graph[V, E])*): Grammar[V, E] = {
    Grammar(axiom, rules.map{ case (label, graph) => label -> graph }.toMap)
  }

  class MapContains(production: (Label, Graph[_,_])) extends ArgumentMatcher[Map[Label, Graph[_,_]]] {
    def matches(map: Any): Boolean = map.asInstanceOf[Map[Label, Graph[_,_]]].toList.contains(production)
  }
  object MapContains {
    def apply(production: (Label, Graph[_,_])) = argThat(new MapContains(production))
  }
}
