package aiolia.test

import aiolia.graph._

import Helpers._

class GraphSpec extends org.specs2.mutable.Specification {
  //TODO: test dsl
  "vertex" >> {
    "toString" >> {
      v(2).toString mustEqual "2"
    }
  }

  "edge" >> {
    "toString" >> {
      e(2 -> 3).toString mustEqual "2 -> 3"
    }
    "contains vertex" >> {
      (e(2 -> 3) contains v(1)) must beFalse
      (e(2 -> 3) contains v(2)) must beTrue
      (e(2 -> 3) contains v(3)) must beTrue
      (e(2 -> 3) contains v(4)) must beFalse
    }
  }

  "nonterminal" >> {
    "contains" >> {
      NT(1, (1, 2, 3)).contains(v(0)) must beFalse
      NT(1, (1, 2, 3)).contains(v(1)) must beTrue
      NT(1, (1, 2, 3)).contains(v(2)) must beTrue
      NT(1, (1, 2, 3)).contains(v(3)) must beTrue
      NT(1, (1, 2, 3)).contains(v(4)) must beFalse
    }
    "toString" >> {
      NT(2, (1, 7, 3, 4)).toString mustEqual "[2:1-7-3-4]"
    }
  }

  "graph" >> {
    "toString" >> {
      "simple" >> {
        graph(V(0 to 5), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3)).toString mustEqual "Graph(V(0 1 2 3 4 5), E(1 -> 0, 1 -> 2, 2 -> 3, 2 -> 4, 3 -> 5, 5 -> 3))"
      }
      "with vertexData" >> {
        graph(V(0 to 1), E(1 -> 0), vertexData(0 -> "wurst", 1 -> "katapult")).toString mustEqual "Graph(V(0 1), E(1 -> 0), {0: wurst, 1: katapult})"
      }
      "with edgeData" >> {
        graph(V(0 to 1), E(1 -> 0, 0 -> 1), ed = edgeData((1 -> 0) -> "kanone", (0 -> 1) -> "salat")).toString mustEqual "Graph(V(0 1), E(0 -> 1, 1 -> 0), {0->1: salat, 1->0: kanone})"
      }
      "with both data" >> {
        graph(V(0 to 1), E(1 -> 0, 0 -> 1),
          vertexData(0 -> "wurst", 1 -> "katapult"),
          edgeData((1 -> 0) -> "kanone", (0 -> 1) -> "salat")).toString mustEqual "Graph(V(0 1), E(0 -> 1, 1 -> 0), {0: wurst, 1: katapult}, {0->1: salat, 1->0: kanone})"
      }
      "with nonTerminals" >> todo
      "with connectors" >> todo
    }

    "assertions" >> {
      "edges" >> {
        graph(V(1), E(1 -> 2)) must throwAn[AssertionError]
        graph(V(2), E(1 -> 2)) must throwAn[AssertionError]
      }
      "self loops" >> todo
      "vertex data" >> { graph(V(), vd = vertexData(1 -> 5)) must throwAn[AssertionError] }
      "edge data" >> {
        graph(V(1, 2), ed = edgeData((1 -> 2) -> 5)) must throwAn[AssertionError]
        graph(V(), ed = edgeData((1 -> 2) -> 5)) must throwAn[AssertionError]
      }
      "nonTerminals" >> {
        graph(V(1), nts = List(NT(1, (1, 2, 3)))) must throwAn[AssertionError]
      }
      "connectors" >> {
        graph(V(1), c = VL(2)) must throwAn[AssertionError]
        graph(V(1), c = VL(2, 1)) must throwAn[AssertionError]
      }
    }

    "traversal accessors" >> {
      val g = graph(
        V(0 to 6),
        E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3),
        nts = List(NT(1), NT(2, (1)), NT(3, (1, 5)), NT(3, (1, 5)), NT(4, (2, 3, 5)))
      )
      "successors" >> {
        g.successors(v(0)) mustEqual V()
        g.successors(v(1)) mustEqual V(0, 2)
        g.successors(v(2)) mustEqual V(4, 3)
        g.successors(v(3)) mustEqual V(5)
        g.successors(v(4)) mustEqual V()
        g.successors(v(5)) mustEqual V(3)
        g.successors(v(6)) mustEqual V()
        g.successors(v(7)) must throwAn[AssertionError]
      }

      "predecessors" >> {
        g.predecessors(v(0)) mustEqual V(1)
        g.predecessors(v(1)) mustEqual V()
        g.predecessors(v(2)) mustEqual V(1)
        g.predecessors(v(3)) mustEqual V(2, 5)
        g.predecessors(v(4)) mustEqual V(2)
        g.predecessors(v(5)) mustEqual V(3)
        g.predecessors(v(6)) mustEqual V()
        g.predecessors(v(7)) must throwAn[AssertionError]
      }

      "incoming edges" >> {
        g.incomingEdges(v(0)) mustEqual E(1 -> 0)
        g.incomingEdges(v(1)) mustEqual E()
        g.incomingEdges(v(2)) mustEqual E(1 -> 2)
        g.incomingEdges(v(3)) mustEqual E(2 -> 3, 5 -> 3)
        g.incomingEdges(v(4)) mustEqual E(2 -> 4)
        g.incomingEdges(v(5)) mustEqual E(3 -> 5)
        g.incomingEdges(v(6)) mustEqual E()
        g.incomingEdges(v(7)) must throwAn[AssertionError]
      }

      "outgoing edges" >> {
        g.outgoingEdges(v(0)) mustEqual E()
        g.outgoingEdges(v(1)) mustEqual E(1 -> 0, 1 -> 2)
        g.outgoingEdges(v(2)) mustEqual E(2 -> 4, 2 -> 3)
        g.outgoingEdges(v(3)) mustEqual E(3 -> 5)
        g.outgoingEdges(v(4)) mustEqual E()
        g.outgoingEdges(v(5)) mustEqual E(5 -> 3)
        g.outgoingEdges(v(6)) mustEqual E()
        g.outgoingEdges(v(7)) must throwAn[AssertionError]
      }

      "neighbours" >> {
        "over edges" >> {
          g.neighbours(v(0)) mustEqual V(1)
          g.neighbours(v(1)) mustEqual V(0, 2)
          g.neighbours(v(2)) mustEqual V(1, 4, 3)
          g.neighbours(v(3)) mustEqual V(5, 2)
          g.neighbours(v(4)) mustEqual V(2)
          g.neighbours(v(5)) mustEqual V(3)
          g.neighbours(v(6)) mustEqual V()
          g.neighbours(v(7)) must throwAn[AssertionError]
        }
        "form multiple vertices" >> {
          g.neighbours(V(0)) mustEqual V(1)
          g.neighbours(V(1)) mustEqual V(0, 2)
          g.neighbours(V(2)) mustEqual V(1, 4, 3)
          g.neighbours(V(3)) mustEqual V(5, 2)
          g.neighbours(V(4)) mustEqual V(2)
          g.neighbours(V(5)) mustEqual V(3)
          g.neighbours(V(6)) mustEqual V()
          g.neighbours(V(7)) must throwAn[AssertionError]

          g.neighbours(V()) mustEqual V()
          g.neighbours(V(1, 2)) mustEqual V(0, 4, 3)
          g.neighbours(V(5, 3)) mustEqual V(2)
          g.neighbours(V(6, 2, 3, 4)) mustEqual V(5, 1)
          g.neighbours(g.vertices) mustEqual V()
        }
      }

      "incident edges" >> {
        "singe vertex" >> {
          g.incidentEdges(v(0)) mustEqual E(1 -> 0)
          g.incidentEdges(v(1)) mustEqual E(1 -> 0, 1 -> 2)
          g.incidentEdges(v(2)) mustEqual E(1 -> 2, 2 -> 4, 2 -> 3)
          g.incidentEdges(v(3)) mustEqual E(2 -> 3, 5 -> 3, 3 -> 5)
          g.incidentEdges(v(4)) mustEqual E(2 -> 4)
          g.incidentEdges(v(5)) mustEqual E(3 -> 5, 5 -> 3)
          g.incidentEdges(v(6)) mustEqual E()
          g.incidentEdges(v(7)) must throwAn[AssertionError]

        }
        "multiple vertices" >> {
          g.incidentEdges(V(0)) mustEqual E(1 -> 0)
          g.incidentEdges(V(1)) mustEqual E(1 -> 0, 1 -> 2)
          g.incidentEdges(V(2)) mustEqual E(1 -> 2, 2 -> 4, 2 -> 3)
          g.incidentEdges(V(3)) mustEqual E(2 -> 3, 5 -> 3, 3 -> 5)
          g.incidentEdges(V(4)) mustEqual E(2 -> 4)
          g.incidentEdges(V(5)) mustEqual E(3 -> 5, 5 -> 3)
          g.incidentEdges(V(6)) mustEqual E()
          g.incidentEdges(V(7)) must throwAn[AssertionError]

          g.incidentEdges(V()) mustEqual E()
          g.incidentEdges(V(2, 3)) mustEqual E(1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3)
          g.incidentEdges(V(0, 1)) mustEqual E(1 -> 0, 1 -> 2)
          g.incidentEdges(g.vertices) mustEqual g.edges
        }
      }

      "incident nonterminals" >> {
        "singe vertex" >> {
          g.incidentNonTerminals(v(0)) mustEqual List()
          g.incidentNonTerminals(v(1)) mustEqual List(NT(2, (1)), NT(3, (1, 5)), NT(3, (1, 5)))
          g.incidentNonTerminals(v(2)) mustEqual List(NT(4, (2, 3, 5)))
          g.incidentNonTerminals(v(3)) mustEqual List(NT(4, (2, 3, 5)))
          g.incidentNonTerminals(v(4)) mustEqual List()
          g.incidentNonTerminals(v(5)) mustEqual List(NT(3, (1, 5)), NT(3, (1, 5)), NT(4, (2, 3, 5)))
          g.incidentNonTerminals(v(6)) mustEqual List()
          g.incidentNonTerminals(v(7)) must throwAn[AssertionError]
        }
        "multiple vertices" >> {
          g.incidentNonTerminals(V(0)) mustEqual List()
          g.incidentNonTerminals(V(1)) mustEqual List(NT(2, (1)), NT(3, (1, 5)), NT(3, (1, 5)))
          g.incidentNonTerminals(V(2)) mustEqual List(NT(4, (2, 3, 5)))
          g.incidentNonTerminals(V(3)) mustEqual List(NT(4, (2, 3, 5)))
          g.incidentNonTerminals(V(4)) mustEqual List()
          g.incidentNonTerminals(V(5)) mustEqual List(NT(3, (1, 5)), NT(3, (1, 5)), NT(4, (2, 3, 5)))
          g.incidentNonTerminals(V(6)) mustEqual List()
          g.incidentNonTerminals(V(7)) must throwAn[AssertionError]

          g.incidentNonTerminals(V()) mustEqual E()
          g.incidentNonTerminals(V(2, 3)) mustEqual List(NT(4, (2, 3, 5)))
          g.incidentNonTerminals(V(0, 1)) mustEqual List(NT(2, (1)), NT(3, (1, 5)), NT(3, (1, 5)))
          g.incidentNonTerminals(g.vertices) mustEqual List(NT(2, (1)), NT(3, (1, 5)), NT(3, (1, 5)), NT(4, (2, 3, 5))) // without NT(1)
        }
      }

      "induced" >> {
        "edges" >> todo
        "nonterminals" >> todo
        "subgraph" >> todo
        "subgraph with data" >> todo
      }
    }

    "modifications" >> {
      //TODO: what about empty nonterminals in modification operations?

      "add vertex" >> {
        "existing" >> {
          graph(V(0 to 2)) + v(1) must throwAn[AssertionError]
        }
        "nonexisting" >> {
          graph(V(0 to 2)) + v(3) mustEqual graph(V(0 to 3))
        }
      }

      "add edge" >> {
        "existing" >> {
          graph(V(0 to 2), E(1 -> 2)) + e(1 -> 2) must throwAn[AssertionError]
        }
        "nonexisting" >> {
          graph(V(0 to 2), E(1 -> 2)) + e(0 -> 2) mustEqual graph(V(0 to 2), E(1 -> 2, 0 -> 2))
        }
      }

      "add nonterminal" >> {
        "existing" >> {
          graph(V(0 to 2), nts = List(NT(1, (1, 2)))) + NT(1, (1, 2)) mustEqual graph(V(0 to 2), nts = List(NT(1, (1, 2)), NT(1, (1, 2))))
        }
        "nonexisting" >> {
          graph(V(0 to 2), nts = List(NT(1, (1, 2)))) + NT(2, (0, 2)) mustEqual graph(V(0 to 2), nts = List(NT(2, (0, 2)), NT(1, (1, 2))))
        }
      }

      val g = graph(
        V(0 to 4),
        E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2),
        vertexData(3 -> "a", 4 -> "b"),
        edgeData((1 -> 3) -> 17L, (0 -> 3) -> -15L, 0 -> 4 -> 18L),
        List(NT(1, (0, 2)), NT(2, (3, 2))),
        c = C(0, 1, 2)
      )

      "remove vertex" >> {
        "from empty graph" >> {
          graph() - v(1) must throwA[AssertionError]
        }
        "nonexisting vertex" >> {
          g - v(17) must throwA[AssertionError]
        }
        "connector vertex" >> {
          g - v(2) must throwA[AssertionError]
        }

        "existing vertex with edges and vertexData" >> {
          g - v(4) mustEqual graph(
            V(0 to 3),
            E(0 -> 3, 1 -> 3),
            vertexData(3 -> "a"),
            edgeData((1 -> 3) -> 17L, (0 -> 3) -> -15L),
            List(NT(1, (0, 2)), NT(2, (3, 2))), c = C(0, 1, 2)
          )
        }

        "existing vertex with edgedata and nonterminals" >> {
          g - v(3) mustEqual graph(
            V(0, 1, 2, 4),
            E(0 -> 4, 4 -> 2),
            vertexData(4 -> "b"),
            edgeData(0 -> 4 -> 18L),
            nts = List(NT(1, (0, 2))), c = C(0, 1, 2)
          )
        }
      }

      "remove edge" >> {
        "from empty graph" >> {
          graph() - e(1 -> 2) must throwA[AssertionError]
        }
        "nonexisting edge" >> {
          g - e(2 -> 4) must throwA[AssertionError]
        }

        "existing edge" >> {
          g - e(4 -> 2) mustEqual graph(
            V(0 to 4),
            E(0 -> 3, 1 -> 3, 0 -> 4),
            vertexData(3 -> "a", 4 -> "b"),
            edgeData((1 -> 3) -> 17L, (0 -> 3) -> -15L, 0 -> 4 -> 18L),
            nts = List(NT(1, (0, 2)), NT(2, (3, 2))), c = C(0, 1, 2)
          )
        }

        "existing edge with data" >> {
          g - e(0 -> 3) mustEqual graph(
            V(0 to 4),
            E(1 -> 3, 0 -> 4, 4 -> 2),
            vertexData(3 -> "a", 4 -> "b"),
            edgeData((1 -> 3) -> 17L, 0 -> 4 -> 18L),
            List(NT(1, (0, 2)), NT(2, (3, 2))), c = C(0, 1, 2)
          )
        }
      }

      "remove nonterminal" >> {
        "from empty graph" >> {
          graph() - NT(1) must throwA[AssertionError]
        }

        "existing nonterminal" >> {
          g - NT(1, (0, 2)) mustEqual graph(
            V(0 to 4),
            E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2),
            vertexData(3 -> "a", 4 -> "b"),
            edgeData((1 -> 3) -> 17L, (0 -> 3) -> -15L, 0 -> 4 -> 18L),
            List(NT(2, (3, 2))), c = C(0, 1, 2)
          )
        }

        "multiple nonterminals with same label" >> {
          val g = graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2), nts = List(NT(1, (0, 2)), NT(1, (3, 2))), c = C(0, 1, 2))
          g - NT(1, (3, 2)) mustEqual graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2), nts = List(NT(1, (0, 2))), c = C(0, 1, 2))
        }

        "multiple nonterminals with same label connected to same vertices" >> {
          val g = graph(V(0 to 2), nts = List(NT(1, (0, 2, 1)), NT(1, (0, 2, 1))), c = C(0, 1))
          g - NT(1, (0, 2, 1)) mustEqual graph(V(0 to 2), nts = List(NT(1, (0, 2, 1))), c = C(0, 1))
        }

        "nonexisting nonterminal" >> {
          g - NT(3) must throwA[AssertionError]
        }
      }

      "remove subgraph" >> {
        "assertions" >> todo
        "full example" >> todo
        "with data" >> todo
      }
    }

    "detect cycle" >> {
      "no cycle" >> {
        val g = graph(V(0 to 5), E(0 -> 1, 0 -> 2, 1 -> 3, 2 -> 4, 2 -> 5))
        g.hasCycle must beFalse
      }
      "undirected cycle" >> {
        val g = graph(V(0 to 4), E(0 -> 1, 2 -> 1, 2 -> 3, 3 -> 4, 4 -> 1))
        g.hasCycle must beFalse
      }
      "directed cycle" >> {
        val g = graph(V(0 to 4), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1))
        g.hasCycle must beTrue
      }
      "small graph cycle" >> {
        val g = graph(V(0 to 5), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3))
        g.hasCycle must beTrue
      }
    }

    "depth first search" >> {
      "nonexistent vertex" >> {
        graph().depthFirstSearch(v(17)).toList must throwAn[AssertionError]
      }
      "one vertex" >> {
        val g = graph(V(0 to 0))
        g.depthFirstSearch(v(0)).toList mustEqual VL(0)
      }
      "directed cycle" >> {
        val g = graph(V(0 to 3), E(0 -> 1, 0 -> 2, 1 -> 3, 3 -> 2, 3 -> 0, 2 -> 1))
        g.depthFirstSearch(v(0), revSort = _.toList.sortBy(-_.label)).toList mustEqual VL(0, 1, 3, 2)
      }
      "undirected cycle" >> {
        val g = graph(V(0 to 3), E(0 -> 1, 0 -> 2, 1 -> 3, 3 -> 2))
        g.depthFirstSearch(v(0), revSort = _.toList.sortBy(-_.label)).toList mustEqual VL(0, 1, 3, 2)
        g.depthFirstSearch(v(0), revSort = _.toList.sortBy(_.label)).toList mustEqual VL(0, 2, 1, 3)
      }
    }
  }
}
