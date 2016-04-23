package aiolia.test

import aiolia.Grammar
import aiolia.graph._
import aiolia.graph.types._

import Helpers._

class GrammarSpec extends org.specs2.mutable.Specification {
  "graph grammar" >> {
    "expand deterministic" >> {
      val h1 = NT(1, (0, 1, 2))
      val h2 = NT(1, (0, 2, 1))
      val axiom = graph(V(0 to 2), nts = List(h1, h2))

      val g = grammar(
        axiom,
        1 -> cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1))
      )

      val e1 = g.expand
      val e2 = g.expand
      e1 mustEqual e2
    }

    "expand edges" >> {
      val h1 = NT(1, (0, 1, 2))
      val h2 = NT(1, (0, 2, 1))
      val axiom = graph(V(0 to 2), nts = List(h1, h2))

      val g = grammar(
        axiom,
        1 -> cgraph(C(0, 1, 2), V(0 to 4), E(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1))
      )

      g.expand mustEqual graph(V(0 to 6), E(2 -> 4, 2 -> 3, 1 -> 5, 4 -> 3, 1 -> 6, 6 -> 5, 0 -> 1, 5 -> 2, 0 -> 2, 3 -> 1))
    }

    "expand nonterminals" >> {
      val h1 = NT(1, (0, 1, 2, 3))
      val axiom = graph(V(0 to 3), nts = List(h1))

      val g = grammar(
        axiom,
        1 -> cgraph(C(0, 1, 2, 3), V(0 to 3), E(0 -> 1, 2 -> 3), nts = List(NT(2, (0, 1, 2)))),
        2 -> cgraph(C(0, 1, 2), V(0 to 2), E(0 -> 2, 1 -> 2))
      )

      g.expand mustEqual graph(V(0 to 3), E(0 -> 1, 0 -> 2, 1 -> 2, 2 -> 3))
    }

    "empty nonterminal axiom" >> {
      val g = grammar(
        A(1),
        1 -> cgraph(Nil, V(0 to 2), E(0 -> 1, 1 -> 2, 2 -> 1))
      )

      g.expand mustEqual graph(V(0 to 2), E(0 -> 1, 1 -> 2, 2 -> 1))
    }

    "redundant replacements" >> {
      val h1 = NT(1, (0, 1))
      val h2 = NT(1, (0, 1))
      val axiom = graph(V(0 to 1), nts = List(h1, h2))

      val g = grammar(
        axiom,
        1 -> cgraph(C(0, 1), V(0 to 1), E(0 -> 1, 1 -> 0))
      )

      g.expand mustEqual graph(V(0 to 1), E(0 -> 1, 1 -> 0))
    }

    "run in circles" >> {
      val g = grammar(
        A(1),
        1 -> cgraph(Nil, V(0 to 1), nts = List(NT(2, (0, 1)))),
        2 -> cgraph(C(0, 1), V(0 to 1), nts = List(NT(3, (0, 1)), NT(3, (1, 0)))),
        3 -> cgraph(C(0, 1), V(0 to 2), nts = List(NT(4, (0, 2)), NT(4, (2, 1)))),
        4 -> cgraph(C(0, 1), V(0 to 2), E(0 -> 2, 2 -> 1))
      )

      g.expand mustEqual graph(V(0 to 7), E(4 -> 2, 5 -> 1, 6 -> 3, 1 -> 6, 3 -> 7, 7 -> 0, 0 -> 4, 2 -> 5))
    }

    "redundant nonterminal" >> {
      val g = grammar(
        A(1),
        1 -> cgraph(Nil, V(0 to 1), nts = List(NT(2, (0, 1)))),
        2 -> cgraph(C(0, 1), V(0 to 1), nts = List(NT(3, (0, 1)), NT(3, (0, 1)))),
        3 -> cgraph(C(0, 1), V(0 to 2), E(0 -> 2, 2 -> 1))
      )

      g.expand mustEqual graph(V(0 to 3), E(0 -> 2, 2 -> 1, 0 -> 3, 3 -> 1))
    }

    "merge vertex data" >> {
      val h1 = NT(1, (0, 1))
      val axiom = graph(V(0 to 1), vd = vertexData(0 -> 5, 1 -> 6), nts = List(h1))

      val g = grammar(
        axiom,
        1 -> cgraph(C(0, 1), V(0 to 2), E(0 -> 2, 2 -> 1), vd = vertexData(2 -> 7))
      )

      g.expand mustEqual graph(V(0 to 2), E(0 -> 2, 2 -> 1), vd = vertexData(0 -> 5, 1 -> 6, 2 -> 7))
    }

    "merge edge data" >> {
      val h1 = NT(1, (0, 1))
      val axiom = graph(V(0 to 2), E(1 -> 2), ed = edgeData((1 -> 2) -> "Wurst"), nts = List(h1))

      val g = grammar(
        axiom,
        1 -> cgraph(C(0, 1), V(0 to 2), E(0 -> 2, 2 -> 1), ed = edgeData((2 -> 1) -> "Worst"))
      )

      g.expand mustEqual graph(V(0 to 3), E(1 -> 2, 0 -> 3, 3 -> 1), ed = edgeData(((1 -> 2) -> "Wurst"), (3 -> 1) -> "Worst"))
    }

    "redundant vertex data" >> {
      val h1 = NT(1, (0, 1))
      val h2 = NT(1, (0, 1))
      val axiom = graph(V(0 to 1), nts = List(h1, h2))

      val g = grammar(
        axiom,
        1 -> cgraph(C(0, 1), V(0 to 2), E(0 -> 2, 2 -> 1), vd = vertexData(2 -> 1))
      )

      g.expand mustEqual graph(V(0 to 3), E(0 -> 2, 2 -> 1, 0 -> 3, 3 -> 1), vd = vertexData(3 -> 1, 2 -> 1))
    }

    "redundant edge data" >> {
      val h1 = NT(1, (0, 1))
      val h2 = NT(1, (0, 1))
      val axiom = graph(V(0 to 1), nts = List(h1, h2))

      val g = grammar(
        axiom,
        1 -> cgraph(C(0, 1), V(0 to 2), E(0 -> 2, 2 -> 1), ed = edgeData((2 -> 1) -> "Worst"))
      )

      g.expand mustEqual graph(V(0 to 3), E(0 -> 2, 2 -> 1, 0 -> 3, 3 -> 1), ed = edgeData(((2 -> 1) -> "Worst"), (3 -> 1) -> "Worst"))
    }

    "grammar can only have rhs nonterminals that have a corresponding lhs" >> {
      "unknown nonterminal label" >> {
        grammar(
          A(1),
          1 -> cgraph(NT(15, (0, 1)))
        ) must throwAn[AssertionError]
      }

      "different nonterminal signature" >> {
        grammar(
          A(1),
          1 -> cgraph(Nil, V(0 to 2), nts = List(NT(2, (0, 1)))),
          2 -> cgraph(C(0, 1, 2), V(0 to 2), E(0 -> 2, 1 -> 2))
        ) must throwAn[AssertionError]
      }

      "signature does not need to match node ids" >> {
        grammar(
          A(1),
          1 -> cgraph(Nil, V(0 to 2), nts = List(NT(2, (0, 1)))),
          2 -> cgraph(C(2, 3), V(2, 3), E(2 -> 3))
        ) must not(throwAn[AssertionError])
      }
    }

    // A graph can only set data on its own vertices/edges and a multi
    // pointed graph may not set data for its input or output vertices.
    // => A projection can only set data for its own nodes.
    // XXX: Do graphs need to set the data for all vertices and edges?
    "projections only operate on local data" >> {
      "vertex data should only contain existing vertices" >> {
        graph(V(), E(0 -> 1), vd = vertexData(2 -> 200)) must throwAn[AssertionError]
      }

      "edge data should only contain existing edges" >> {
        graph(V(), E(0 -> 1), ed = edgeData((1 -> 2) -> 200)) must throwAn[AssertionError]
      }

      "disallow overriding data of input vertex" >> {
        graph(V(), E(), vd = vertexData(0 -> "bier"), c = C(0, 1)) must throwAn[AssertionError]
      }

      "disallow overriding data of output vertex" >> {
        graph(V(), E(), vd = vertexData(1 -> "wein"), c = C(0, 1)) must throwAn[AssertionError]
      }
    }

    "axiom has to have ids 0 until |vertices|" >> {
      val axiom = graph(V(0, 2))
      Grammar(axiom) must throwAn[AssertionError]
    }

    "grammar cannot have cycles" >> {
      grammar(A(1), 1 -> cgraph(NT(2)), 2 -> cgraph(NT(1))) must throwAn[AssertionError]
    }

    "cleanup unused production rules" >> {
      val g = grammar(
        A(1),
        1 -> cgraph(),
        2 -> cgraph()
      )
      g.cleanup mustEqual grammar(A(1), 1 -> cgraph())
    }
    "cleanup many unused production rules" >> {
      val g = grammar(
        A(1),
        1 -> cgraph(NT(2)),
        2 -> cgraph(),

        3 -> cgraph(NT(4)),
        4 -> cgraph(),

        5 -> cgraph(NT(6)),
        6 -> cgraph()
      )
      g.cleanup mustEqual grammar(
        A(1),
        1 -> cgraph(NT(2)),
        2 -> cgraph()
      )
    }
    "cleanup unused production rules (tree)" >> {
      val axiom = graph(NT(1), NT(2))
      val g = grammar(
        axiom,
        1 -> cgraph(),
        2 -> cgraph(),
        3 -> cgraph()
      )
      g.cleanup mustEqual grammar(axiom, 1 -> cgraph(), 2 -> cgraph())
    }
  }
}