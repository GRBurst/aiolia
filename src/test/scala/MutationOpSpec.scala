package aiolia.test

import aiolia.Grammar
import aiolia.graph._
import aiolia.graph.dsl._
import aiolia.graph.types._
import aiolia.helpers.Random
import aiolia.mutations._

import Helpers._

class MutationOpSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {
  args.report(failtrace = true) // prints more stacktrace information when grouping matchers in helper functions
  "mutation operator" >> {
    "remove random vertex" >> {
      "from minimal grammar" >> {
        val random = mock[Random]
        random.selectOpt(Map.empty) returns None
        RemoveVertex(Grammar.minimal, random) mustEqual None
      }

      "from grammar" >> {
        val random = mock[Random]

        val h1 = nt(1, (0, 1, 2))
        val h2 = nt(1, (0, 2, 1))
        val axiom = graph(V(0 to 2), nts = List(h1, h2))

        val rhsA1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(nt(2, (0, 2))))
        val rhsA2 = cgraph(C(0, 2), V(0 to 2), E(0 -> 1, 1 -> 2))
        val g = grammar(axiom, 1 -> rhsA1, 2 -> rhsA2)

        random.selectOpt(Map(1 -> rhsA1, 2 -> rhsA2)) returns Some((2 -> rhsA2))
        random.select(V(1)) returns v(1)

        RemoveVertex(g, random) mustEqual Some(grammar(
          axiom,
          1 -> rhsA1,
          2 -> cgraph(C(0, 2), V(0, 2))
        ))
      }
    }

    "remove random edge" >> {
      "from grammar" >> {
        val random = mock[Random]

        val h1 = nt(1, (0, 1, 2))
        val h2 = nt(1, (0, 2, 1))
        val axiom = graph(V(0 to 2), nts = List(h1, h2))

        val rhsA1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(nt(2, (0, 2))))
        val rhsA2 = cgraph(C(0, 2), V(0 to 2), E(0 -> 1, 1 -> 2))
        val g = grammar(axiom, 1 -> rhsA1, 2 -> rhsA2)

        random.selectOpt(Map(1 -> rhsA1, 2 -> rhsA2)) returns Some((1 -> rhsA1))
        random.select(E(2 -> 4, 2 -> 3, 4 -> 1, 0 -> 2, 3 -> 4)) returns e(2 -> 4) // this edge will be removed from the graph in rhsA1

        RemoveEdge(g, random) mustEqual Some(grammar(
          axiom,
          1 -> cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 4 -> 1), nts = List(nt(2, (0, 2)))),
          2 -> rhsA2
        ))
      }

      "from minimal grammar" >> {
        val random = mock[Random]
        random.selectOpt(Map.empty) returns None
        RemoveEdge(Grammar.minimal, random) mustEqual None
      }

    }

    "inline random nonterminal" >> {
      "on minimal grammar" >> {
        val random = mock[Random]
        random.selectOpt(Map.empty) returns None
        InlineNonTerminal(Grammar.minimal, random) mustEqual None
      }

      "on grammar" >> {
        val random = mock[Random].smart

        val axiom = graph(
          V(0 to 2),
          nts = List(
            nt(1, (0, 1, 2)),
            nt(1, (0, 2, 1))
          )
        )

        val rhs1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(nt(2, (0, 2)))) // the nonTerminal in this graph will be inlined with rhs2
        val rhs2 = cgraph(C(0, 2), V(0 to 2), E(0 -> 1, 1 -> 2))
        val g = grammar(
          axiom,
          1 -> rhs1,
          2 -> rhs2
        )

        random.selectOpt(Map(1 -> rhs1)) returns Some((1 -> rhs1))
        random.select(List(nt(2, (0, 2)))) returns nt(2, (0, 2)) // inline this nonTerminal

        InlineNonTerminal(g, random) mustEqual Some(grammar(
          axiom,
          1 -> cgraph(C(0, 1, 2), V(0 to 5), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1, 0 -> 5, 5 -> 2)),
          2 -> rhs2
        ))
      }.pendingUntilFixed("- some Mockito on maps issue")
    }

    "extract NonTerminal" >> {
      import ExtractNonTerminal.extract
      //TODO: special case: extract vertex with degree 0
      //TODO: extract all vertices
      //TODO: neigbours over nonterminals (between all combinations of connectors and vertices)
      //TODO: data
      //
      def t[V, E](source: Graph[V, E], vertices: Set[Vertex], wantedNewSource: Graph[V, E], wantedExtracted: Graph[V, E], newLabel: Label) = {
        val (newSource, extracted) = extract(source, vertices, newLabel)

        newSource.copy(nonTerminals = Nil) mustEqual wantedNewSource.copy(nonTerminals = Nil)
        extracted.copy(connectors = Nil) mustEqual wantedExtracted.copy(connectors = Nil)

        newSource.nonTerminals.size mustEqual source.nonTerminals.size + 1
        val newNonTerminal = (newSource.nonTerminals diff source.nonTerminals).head
        newNonTerminal.label mustEqual newLabel
        newNonTerminal.connectors mustEqual extracted.connectors
        extracted.connectors.toSet mustEqual wantedExtracted.connectors.toSet

        (newSource.nonTerminals diff List(newNonTerminal)) mustEqual wantedNewSource.nonTerminals

        (newSource.vertices ++ extracted.vertices) mustEqual source.vertices
        (newSource.edges ++ extracted.edges) mustEqual source.edges
        (source.connectors.toSet subsetOf newSource.connectors.toSet) must beTrue
      }

      "connected" >> {
        val source = cgraph(C(3, 4), V(3, 4, 5, 6), E(4 -> 3, 3 -> 5, 5 -> 6, 4 -> 5))
        val newLabel = 17
        def c[V, E](vertices: Set[Vertex], wantedNewSource: Graph[V, E], wantedExtracted: Graph[V, E]) = t(source, vertices, wantedNewSource, wantedExtracted, newLabel)

        c(V(3), cgraph(C(3, 4), V(3, 4, 5, 6), E(4 -> 5, 5 -> 6)), cgraph(C(3, 4, 5), V(3, 4, 5), E(4 -> 3, 3 -> 5)))
        c(V(4), cgraph(C(3, 4), V(3, 4, 5, 6), E(3 -> 5, 5 -> 6)), cgraph(C(3, 4, 5), V(3, 4, 5), E(4 -> 3, 4 -> 5)))
        c(V(5), cgraph(C(3, 4), V(3, 4, 6), E(4 -> 3)), cgraph(C(3, 4, 6), V(3, 4, 5, 6), E(3 -> 5, 4 -> 5, 5 -> 6)))
        c(V(6), cgraph(C(3, 4), V(3, 4, 5), E(3 -> 5, 4 -> 3, 4 -> 5)), cgraph(C(5), V(5, 6), E(5 -> 6)))
        c(V(3, 4), cgraph(C(3, 4), V(3, 4, 5, 6), E(5 -> 6)), cgraph(C(3, 4, 5), V(3, 4, 5), E(3 -> 5, 4 -> 3, 4 -> 5)))
        c(V(3, 5), cgraph(C(3, 4), V(3, 4, 6), E()), cgraph(C(3, 4, 6), V(3, 4, 5, 6), E(4 -> 3, 3 -> 5, 5 -> 6, 4 -> 5)))
        c(V(3, 6), cgraph(C(3, 4), V(3, 4, 5), E(4 -> 5)), cgraph(C(3, 4, 5), V(3, 4, 5, 6), E(4 -> 3, 3 -> 5, 5 -> 6)))
        c(V(4, 5), cgraph(C(3, 4), V(3, 4, 6), E()), cgraph(C(3, 4, 6), V(3, 4, 5, 6), E(4 -> 3, 3 -> 5, 5 -> 6, 4 -> 5)))
        c(V(4, 6), cgraph(C(3, 4), V(3, 4, 5), E(3 -> 5)), cgraph(C(3, 4, 5), V(3, 4, 5, 6), E(4 -> 3, 5 -> 6, 4 -> 5)))
        c(V(5, 6), cgraph(C(3, 4), V(3, 4), E(4 -> 3)), cgraph(C(3, 4), V(3, 4, 5, 6), E(3 -> 5, 5 -> 6, 4 -> 5)))
      }
    }
  }
}
