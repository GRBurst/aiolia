package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraphgrammar._
import aiolia.test.Helpers._
import aiolia.helpers.Random

class MutationOperatorSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {
  "mutation operator" >> {
    "remove random vertex" >> {
      "from empty grammar" >> {
        val random = mock[Random]
        Mutation.removeRandomVertex(Grammar(A(1)), random) mustEqual None
      }

      "from grammar" >> {
        val random = mock[Random]

        val h1 = NT(1, (0, 1, 2))
        val h2 = NT(1, (0, 2, 1))
        val axiom = graph(V(0 to 2), nts = List(h1, h2))

        val rhsA1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(NT(2, (0, 2))))
        val rhsA2 = cgraph(C(0, 2), V(0 to 2), E(0 -> 1, 1 -> 2))
        val g = grammar(axiom, 1 -> rhsA1, 2 -> rhsA2)

        random.select(g.productions) returns (2 -> rhsA2)
        random.select(Set(Vertex(1))) returns Vertex(1)

        Mutation.removeRandomVertex(g, random) mustEqual Some(grammar(
          axiom,
          1 -> rhsA1,
          2 -> cgraph(C(0, 2), V(0, 2))
        ))
      }

      "select rule without removable vertices" >> {
        val random = mock[Random]

        val h1 = NT(1, (0, 1, 2))
        val h2 = NT(1, (0, 2, 1))
        val axiom = graph(V(0 to 2), nts = List(h1, h2))

        val rhsA1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(NT(2, (0, 2))))
        val rhsA2 = cgraph(C(0, 1), V(0 to 1), E(0 -> 1))
        val g = grammar(axiom, 1 -> rhsA1, 2 -> rhsA2)

        random.select(g.productions) returns (2 -> rhsA2)

        Mutation.removeRandomVertex(g, random) mustEqual None
      }
    }
    "remove random edge" >> {
      "from grammar" >> {
        val random = mock[Random]

        val h1 = NT(1, (0, 1, 2))
        val h2 = NT(1, (0, 2, 1))
        val axiom = graph(V(0 to 2), nts = List(h1, h2))

        val rhsA1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(NT(2, (0, 2))))
        val rhsA2 = cgraph(C(0, 2), V(0 to 2), E(0 -> 1, 1 -> 2))
        val g = grammar(axiom, 1 -> rhsA1, 2 -> rhsA2)

        random.select(g.productions) returns (1 -> rhsA1)
        random.select(E(2 -> 4, 2 -> 3, 4 -> 1, 0 -> 2, 3 -> 4)) returns e(2 -> 4) // this edge will be removed from the graph in rhsA1

        Mutation.removeRandomEdge(g, random) mustEqual Some(grammar(
          axiom,
          1 -> cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 4 -> 1), nts = List(NT(2, (0, 2)))),
          2 -> rhsA2
        ))
      }

      "from empty grammar" >> {
        val random = mock[Random]
        Mutation.removeRandomEdge(Grammar(A(1)), random) mustEqual None
      }

      "select rule without removable edges" >> {
        val random = mock[Random]

        val h1 = NT(1, (0, 1, 2))
        val h2 = NT(1, (0, 2, 1))
        val axiom = graph(V(0 to 2), nts = List(h1, h2))

        val rhsA1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(NT(2, (0, 2))))
        val rhsA2 = cgraph(C(0, 1), V(0 to 1))
        val g = grammar(axiom, 1 -> rhsA1, 2 -> rhsA2)

        random.select(g.productions) returns (2 -> rhsA2)

        Mutation.removeRandomVertex(g, random) mustEqual None
      }
    }

    "inline random hyperedge" >> {
      "on empty grammar" >> {
        val random = mock[Random]
        Mutation.inlineRandomNonTerminal(Grammar(A(1)), random) mustEqual None
      }

      "on grammar" >> {
        val random = mock[Random]

        val axiom = graph(
          V(0 to 2),
          nts = List(
            NT(1, (0, 1, 2)),
            NT(1, (0, 2, 1))
          )
        )

        val rhs1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(NT(2, (0, 2)))) // the nonTerminal in this graph will be inlined with rhs2
        val rhs2 = cgraph(C(0, 2), V(0 to 2), E(0 -> 1, 1 -> 2))
        val g = grammar(
          axiom,
          1 -> rhs1,
          2 -> rhs2
        )

        random.select(g.productions) returns (1 -> rhs1)
        random.select(List(NT(2, (0, 2)))) returns NT(2, (0, 2)) // inline this nonTerminal

        Mutation.inlineRandomNonTerminal(g, random) mustEqual Some(grammar(
          axiom,
          1 -> cgraph(C(0, 1, 2), V(0 to 5), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1, 0 -> 5, 5 -> 2)),
          2 -> rhs2
        ))
      }

      "on grammar rule without nonTerminal" >> {
        val random = mock[Random]

        val h1 = NT(1, (0, 1, 2))
        val h2 = NT(1, (0, 2, 1))
        val axiom = graph(V(0 to 2), nts = List(h1, h2))

        val rhsA1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(NT(2, (0, 2))))
        val rhsA2 = cgraph(C(0, 2), V(0 to 2), E(0 -> 1, 1 -> 2))
        val g = grammar(axiom, 1 -> rhsA1, 2 -> rhsA2)

        random.select(g.productions) returns (2 -> rhsA2)

        Mutation.inlineRandomNonTerminal(g, random) mustEqual None
      }

    }
  }
}
