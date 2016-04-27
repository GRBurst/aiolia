package aiolia.test

import aiolia.{Grammar, Mutation}
import aiolia.graph._
import aiolia.graph.dsl._
import aiolia.helpers.Random

import Helpers._

class MutationSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {
  "mutation operator" >> {
    "remove random vertex" >> {
      "from minimal grammar" >> {
        val random = mock[Random]
        Mutation.removeVertex(Grammar.minimal, random) mustEqual None
      }

      "from grammar" >> {
        val random = mock[Random]

        val h1 = nt(1, (0, 1, 2))
        val h2 = nt(1, (0, 2, 1))
        val axiom = graph(V(0 to 2), nts = List(h1, h2))

        val rhsA1 = cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), nts = List(nt(2, (0, 2))))
        val rhsA2 = cgraph(C(0, 2), V(0 to 2), E(0 -> 1, 1 -> 2))
        val g = grammar(axiom, 1 -> rhsA1, 2 -> rhsA2)

        random.select(g.productions) returns (2 -> rhsA2)
        random.select(V(1)) returns v(1)

        Mutation.removeVertex(g, random) mustEqual Some(grammar(
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

        random.select(g.productions) returns (1 -> rhsA1)
        random.select(E(2 -> 4, 2 -> 3, 4 -> 1, 0 -> 2, 3 -> 4)) returns e(2 -> 4) // this edge will be removed from the graph in rhsA1

        Mutation.removeEdge(g, random) mustEqual Some(grammar(
          axiom,
          1 -> cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 4 -> 1), nts = List(nt(2, (0, 2)))),
          2 -> rhsA2
        ))
      }

      "from minimal grammar" >> {
        val random = mock[Random]
        Mutation.removeEdge(Grammar.minimal, random) mustEqual None
      }

    }

    "inline random nonterminal" >> {
      "on minimal grammar" >> {
        val random = mock[Random]
        Mutation.inlineNonTerminal(Grammar.minimal, random) mustEqual None
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

        random.select(g.productions) returns (1 -> rhs1)
        random.select(List(nt(2, (0, 2)))) returns nt(2, (0, 2)) // inline this nonTerminal

        Mutation.inlineNonTerminal(g, random) mustEqual Some(grammar(
          axiom,
          1 -> cgraph(C(0, 1, 2), V(0 to 5), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1, 0 -> 5, 5 -> 2)),
          2 -> rhs2
        ))
      }.pendingUntilFixed("- some Mockito on maps issue")

    }
  }
}
