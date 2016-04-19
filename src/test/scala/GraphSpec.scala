package aiolia.graph

import aiolia.test.Helpers._

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
    }

    "traversal accessors" >> {
      val g = graph(V(0 to 5), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3))
      "successors" >> {
        g.successors(v(0)) mustEqual V()
        g.successors(v(1)) mustEqual V(0, 2)
        g.successors(v(2)) mustEqual V(4, 3)
        g.successors(v(3)) mustEqual V(5)
        g.successors(v(4)) mustEqual V()
        g.successors(v(5)) mustEqual V(3)
      }

      "predecessors" >> {
        g.predecessors(v(0)) mustEqual V(1)
        g.predecessors(v(1)) mustEqual V()
        g.predecessors(v(2)) mustEqual V(1)
        g.predecessors(v(3)) mustEqual V(2, 5)
        g.predecessors(v(4)) mustEqual V(2)
        g.predecessors(v(5)) mustEqual V(3)
      }

      "incoming edges" >> {
        g.incomingEdges(v(0)) mustEqual E(1 -> 0)
        g.incomingEdges(v(1)) mustEqual E()
        g.incomingEdges(v(2)) mustEqual E(1 -> 2)
        g.incomingEdges(v(3)) mustEqual E(2 -> 3, 5 -> 3)
        g.incomingEdges(v(4)) mustEqual E(2 -> 4)
        g.incomingEdges(v(5)) mustEqual E(3 -> 5)
      }

      "outgoing edges" >> {
        g.outgoingEdges(v(0)) mustEqual E()
        g.outgoingEdges(v(1)) mustEqual E(1 -> 0, 1 -> 2)
        g.outgoingEdges(v(2)) mustEqual E(2 -> 4, 2 -> 3)
        g.outgoingEdges(v(3)) mustEqual E(3 -> 5)
        g.outgoingEdges(v(4)) mustEqual E()
        g.outgoingEdges(v(5)) mustEqual E(5 -> 3)
      }

      "neighbours" >> {
        "over edges" >> todo
        "form multiple vertices" >> todo
      }

      "incident edges" >> {
        "singe vertex" >> todo
        "multiple vertices" >> todo
      }

      "incident nonterminals" >> {
        "singe vertex" >> todo
        "multiple vertices" >> todo
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
          graph(V(0 to 2)) + Vertex(1) must throwAn[AssertionError]
        }
        "nonexisting" >> {
          graph(V(0 to 2)) + Vertex(3) mustEqual graph(V(0 to 3))
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

      // TODO: clean up redundant tests
      val mphg = graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2), nts = List(NT(1, (0, 2)), NT(2, (3, 2))), c = C(0, 1, 2))
      "remove vertex" >> {
        "from empty graph" >> {
          graph() - v(1) must throwA[AssertionError]
        }

        "existing vertex with edges" >> {
          mphg - v(4) mustEqual graph(V(0 to 3), E(0 -> 3, 1 -> 3), nts = List(NT(1, (0, 2)), NT(2, (3, 2))), c = C(0, 1, 2))
        }
        "existing vertex with nonterminals" >> {
          mphg - v(3) mustEqual graph(V(0, 1, 2, 4), E(0 -> 4, 4 -> 2), nts = List(NT(1, (0, 2))), c = C(0, 1, 2))
        }
        "nonexisting vertex" >> {
          mphg - v(17) must throwA[AssertionError]
        }
        "input vertex" >> {
          mphg - v(0) must throwA[AssertionError]
        }
        "ouput vertex" >> {
          mphg - v(2) must throwA[AssertionError]
        }
      }
      "remove vertex" >> {
        "nonexisting vertex" >> {
          (graph(V(0 to 2)) - v(18)) must throwAn[AssertionError]
        }
        "existing vertex" >> {
          val g = graph(
            V(0 to 2),
            E(0 -> 1, 1 -> 2, 0 -> 2),
            nts = List(NT(1, (0, 2, 1)), NT(2, (2, 0)))
          )
          val wanted = graph(
            V(0, 2),
            E(0 -> 2),
            nts = List(NT(2, (2, 0)))
          )
          (g - Vertex(1)) mustEqual wanted
        }
        "with data" >> {
          val g = graph(
            V(0 to 2),
            E(0 -> 1, 1 -> 2, 0 -> 2),
            vd = vertexData(1 -> "x", 2 -> "y"),
            ed = edgeData((0 -> 1) -> "a", (1 -> 2) -> "b", (0 -> 2) -> "c")
          )
          val wanted = graph(
            V(0, 2),
            E(0 -> 2),
            vd = vertexData(2 -> "y"),
            ed = edgeData((0 -> 2) -> "c")
          )
          (g - Vertex(1)) mustEqual wanted
        }
      }

      "remove edge" >> {
        "from empty graph" >> {
          graph() - e(1 -> 2) must throwA[AssertionError]
        }

        "existing edge" >> {
          mphg - e(4 -> 2) mustEqual graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4), nts = List(NT(1, (0, 2)), NT(2, (3, 2))), c = C(0, 1, 2))
        }

        "nonexisting edge" >> {
          mphg - e(2 -> 4) must throwA[AssertionError]
        }
      }
      "remove edge" >> {
        "nonexisting" >> {
          (graph(V(0 to 2)) - e(18 -> 19)) must throwAn[AssertionError]
        }
        "existing edge" >> {
          (graph(V(0 to 2), E(0 -> 2, 0 -> 1)) - e(0 -> 1)) mustEqual graph(V(0 to 2), E(0 -> 2))
        }
        "with data" >> {
          val g = graph(V(0 to 2), E(0 -> 2, 0 -> 1), ed = edgeData((0 -> 2) -> "a", (0 -> 1) -> "b"))
          (g - e(0 -> 1)) mustEqual graph(V(0 to 2), E(0 -> 2), ed = edgeData((0 -> 2) -> "a"))
        }
      }

      "remove nonterminal" >> {
        "from empty graph" >> {
          graph() - NT(1) must throwA[AssertionError]
        }

        "existing nonterminal" >> {
          mphg - NT(1, (0, 2)) mustEqual graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2), nts = List(NT(2, (3, 2))), c = C(0, 1, 2))
        }

        "multiple nonterminals with same label" >> {
          val mphg = graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2), nts = List(NT(1, (0, 2)), NT(1, (3, 2))), c = C(0, 1, 2))
          mphg - NT(1, (3, 2)) mustEqual graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2), nts = List(NT(1, (0, 2))), c = C(0, 1, 2))
        }

        "multiple nonterminals with same label connected to same vertices" >> {
          val mphg = graph(V(0 to 2), nts = List(NT(1, (0, 2, 1)), NT(1, (0, 2, 1))), c = C(0, 1))
          mphg - NT(1, (0, 2, 1)) mustEqual graph(V(0 to 2), nts = List(NT(1, (0, 2, 1))), c = C(0, 1))
        }

        "nonexisting nonterminal" >> {
          mphg - NT(3) must throwA[AssertionError]
        }
      }
      "remove nonterminal" >> {
        "nonexisting" >> {
          (graph(V(0 to 2)) - NT(18, (19, 20))) must throwAn[AssertionError]
        }
        "existing nonterminal" >> {
          (graph(V(0 to 2), nts = List(NT(1, (0, 2)))) - NT(1, (0, 2))) mustEqual graph(V(0 to 2))
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
