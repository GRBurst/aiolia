package aiolia.test

import aiolia.graph._
import aiolia.graph.dsl._

import Helpers._
import aiolia.helpers.AutoId

class GraphSpec extends org.specs2.mutable.Specification {
  "dsl" >> {
    "vertex" >> {
      v(1) mustEqual Vertex(1)
    }
    "vertex range" >> {
      V(4 to 6) mustEqual Set(Vertex(4), Vertex(5), Vertex(6))
    }
    "vertex set" >> {
      V(1, 2) mustEqual Set(Vertex(1), Vertex(2))
      V() mustEqual Set.empty
    }
    "vertex list" >> {
      VL(1, 2, 3) mustEqual List(Vertex(1), Vertex(2), Vertex(3))
      VL() mustEqual List.empty
    }
    "edge" >> {
      e(3 -> 2) mustEqual Edge(Vertex(3), Vertex(2))
    }
    "edge set" >> {
      E(1 -> 2, 2 -> 3) mustEqual Set(Edge(Vertex(1), Vertex(2)), Edge(Vertex(2), Vertex(3)))
      E() mustEqual Set.empty
    }
    "connectors" >> {
      C(1, 2, 3) mustEqual List(Vertex(1), Vertex(2), Vertex(3))
      C() mustEqual List.empty
    }
    "nonterminal" >> {
      nt(1, (1, 2, 3)) mustEqual NonTerminal(1, VL(1, 2, 3))
      nt(1) mustEqual NonTerminal(1)
      nt(1, List(1)) must throwAn[AssertionError]
    }
    "vertex-data" >> {
      vData(1 -> "hans", 2 -> "peter") mustEqual Map(Vertex(1) -> "hans", Vertex(2) -> "peter")
    }
    "edge-data" >> {
      eData((1 -> 2) -> "wurst", (2 -> 1) -> "fach") mustEqual Map(Edge(Vertex(1), Vertex(2)) -> "wurst", Edge(Vertex(2), Vertex(1)) -> "fach")
    }
  }

  "vertex" >> {
    "toString" >> {
      v(2).toString mustEqual "2"
    }
    "map" >> {
      v(1).map(_ + 1) mustEqual v(2)
    }
  }

  "edge" >> {
    "toString" >> {
      e(2 -> 3).toString mustEqual "2 -> 3"
    }
    "disallow self loops" >> {
      e(1 -> 1) must throwAn[AssertionError]
    }
    "contains vertex" >> {
      (e(2 -> 3) contains v(1)) must beFalse
      (e(2 -> 3) contains v(2)) must beTrue
      (e(2 -> 3) contains v(3)) must beTrue
      (e(2 -> 3) contains v(4)) must beFalse
    }
    "toSet" >> {
      e(1 -> 2).toSet mustEqual V(1, 2)
    }
    "map" >> {
      e(1 -> 2).map(_ + 1) mustEqual e(2 -> 3)
    }
  }

  "nonterminal" >> {
    "connectors must be distinct" >> {
      NonTerminal(1, VL(2, 2)) must throwAn[AssertionError]
    }
    "contains" >> {
      nt(1, (1, 2, 3)).contains(v(0)) must beFalse
      nt(1, (1, 2, 3)).contains(v(1)) must beTrue
      nt(1, (1, 2, 3)).contains(v(2)) must beTrue
      nt(1, (1, 2, 3)).contains(v(3)) must beTrue
      nt(1, (1, 2, 3)).contains(v(4)) must beFalse
    }
    "toString" >> {
      nt(2, (1, 7, 3, 4)).toString mustEqual "[2:1-7-3-4]"
    }
    "map" >> {
      nt(1, (1, 2, 3)).map(_ * 2) mustEqual nt(1, (2, 4, 6))
    }
  }

  "graph" >> {
    "toString" >> {
      "simple" >> {
        graph(V(0 to 5), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3)).toString mustEqual "G(V(0, 1, 2, 3, 4, 5), E(1 -> 0, 1 -> 2, 2 -> 3, 2 -> 4, 3 -> 5, 5 -> 3))"
      }
      "with vertexData" >> {
        graph(V(0 to 1), E(1 -> 0), vData(0 -> "wurst", 1 -> "katapult")).toString mustEqual "G(V(0, 1), E(1 -> 0), {0: wurst, 1: katapult})"
      }
      "with edgeData" >> {
        graph(V(0 to 1), E(1 -> 0, 0 -> 1), ed = eData((1 -> 0) -> "kanone", (0 -> 1) -> "salat")).toString mustEqual "G(V(0, 1), E(0 -> 1, 1 -> 0), {0->1: salat, 1->0: kanone})"
      }
      "with both data" >> {
        graph(V(0 to 1), E(1 -> 0, 0 -> 1),
          vData(0 -> "wurst", 1 -> "katapult"),
          eData((1 -> 0) -> "kanone", (0 -> 1) -> "salat")).toString mustEqual "G(V(0, 1), E(0 -> 1, 1 -> 0), {0: wurst, 1: katapult}, {0->1: salat, 1->0: kanone})"
      }
      "with nonTerminals" >> {
        graph(V(0 to 3), E(1 -> 0, 1 -> 2, 2 -> 3), nts = List(nt(1, (0, 1, 2)), nt(2, (2, 3)), nt(3))).toString mustEqual "G(V(0, 1, 2, 3), E(1 -> 0, 1 -> 2, 2 -> 3), NTS([1:0-1-2], [2:2-3], [3]))"
      }
      "with connectors" >> {
        graph(V(0 to 3), E(1 -> 0, 1 -> 2, 2 -> 3), c = C(0, 3)).toString mustEqual "G(V(0, 1, 2, 3), E(1 -> 0, 1 -> 2, 2 -> 3), C(0-3))"
      }
    }
    "map" >> {
      val g = graph(V(0 to 3), E(0 -> 1, 1 -> 2, 2 -> 3), vData(0 -> "friedrich", 3 -> "friedhelm"), eData((0 -> 1) -> 0, (2 -> 3) -> 1), List(nt(1, (0, 1))), C(2))
      g.map(_ + 1) mustEqual graph(V(1 to 4), E(1 -> 2, 2 -> 3, 3 -> 4), vData(1 -> "friedrich", 4 -> "friedhelm"), eData((1 -> 2) -> 0, (3 -> 4) -> 1), List(nt(1, (1, 2))), C(3))
    }

    "assertions" >> {
      "edges can only connect existing vertices" >> {
        graph(V(1), E(1 -> 2)) must throwAn[AssertionError]
        graph(V(2), E(1 -> 2)) must throwAn[AssertionError]
      }
      "vertex data can only be attached to existing vertices" >> {
        graph(V(), vd = vData(1 -> 5)) must throwAn[AssertionError]
      }
      "edge data can only be attached to existing edges" >> {
        graph(V(1, 2), ed = eData((1 -> 2) -> 5)) must throwAn[AssertionError]
        graph(V(), ed = eData((1 -> 2) -> 5)) must throwAn[AssertionError]
      }
      "nonTerminals can only connect existing vertices" >> {
        graph(V(1), nts = List(nt(1, (1, 2, 3)))) must throwAn[AssertionError]
      }
      "nonTerminals with same label must have the same number of connectors" >> {
        graph(V(1, 2), nts = List(nt(1, (1, 2)), nt(1, (1)))) must throwAn[AssertionError]
      }
      "connectors in graph need to be distinct" >> {
        graph(V(1), c = VL(1, 1)) must throwAn[AssertionError]
        graph(V(1, 2), c = VL(1, 2, 1)) must throwAn[AssertionError]
      }
      "only existing vertices can be used as connectors" >> {
        graph(V(1), c = VL(2)) must throwAn[AssertionError]
        graph(V(1), c = VL(2, 1)) must throwAn[AssertionError]
      }
      "connectors cannot store data" >> {
        cgraph(C(1), V(1, 2), vd = vData(1 -> "a")) must throwAn[AssertionError]
      }
    }

    "nonConnectors" >> {
      cgraph(C(1, 2), V(1, 2, 3, 4)).nonConnectors mustEqual V(3, 4)
    }

    "subGraphOf" >> {
      (graph() subGraphOf graph()) must beTrue
      (graph() subGraphOf graph(V(1, 2), E(1 -> 2))) must beTrue
      (graph(V(1)) subGraphOf graph(V(1, 2), E(1 -> 2))) must beTrue
      (graph(V(1, 2), E(1 -> 2)) subGraphOf graph(V(1, 2), E(1 -> 2))) must beTrue
      (graph(V(1, 2), E(1 -> 2), nts = List(nt(1, (1, 2)))) subGraphOf graph(V(1, 2), E(1 -> 2), nts = List(nt(1, (1, 2))))) must beTrue
      (cgraph(C(1), V(1)) subGraphOf cgraph(C(1), V(1, 2))) must beTrue
      (cgraph(C(1, 2), V(1, 2, 3)) subGraphOf cgraph(C(1, 2, 3), V(1, 2, 3))) must beTrue
      (cgraph(C(2, 4), V(1 to 4)) subGraphOf cgraph(C(1, 2, 3, 4), V(1 to 4))) must beTrue

      (graph(V(3)) subGraphOf graph(V(1, 2), E(1 -> 2))) must beFalse
      (graph(V(1, 2), E(2 -> 1)) subGraphOf graph(V(1, 2), E(1 -> 2))) must beFalse
      (graph(V(1, 2), E(1 -> 2), nts = List(nt(1, (1, 2)))) subGraphOf graph(V(1, 2), E(1 -> 2))) must beFalse
      (cgraph(C(1), V(1)) subGraphOf graph(V(1, 2))) must beFalse
      (cgraph(C(1, 2), V(1, 2)) subGraphOf cgraph(C(1), V(1, 2))) must beFalse
      (cgraph(C(1, 2), V(1, 2)) subGraphOf cgraph(C(2, 1), V(1, 2))) must beFalse
      (cgraph(C(4, 2), V(1 to 4)) subGraphOf cgraph(C(1, 2, 3, 4), V(1 to 4))) must beFalse
    }

    "traversal accessors" >> {
      val g = graph(
        V(0 to 6),
        E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3),
        vData(2 -> "a", 1 -> "b", 3 -> "c"),
        eData((2 -> 3) -> "x", (1 -> 2) -> "y", (1 -> 0) -> "z"),
        List(nt(1), nt(2, (1)), nt(3, (1, 5)), nt(3, (1, 5)), nt(4, (2, 3, 5))),
        C(5)
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

        "over edges form multiple vertices" >> {
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

        "over nonTerminals" >> {
          g.neighboursOverNonTerminals(v(0)) mustEqual V()
          g.neighboursOverNonTerminals(v(1)) mustEqual V(5)
          g.neighboursOverNonTerminals(v(2)) mustEqual V(3, 5)
          g.neighboursOverNonTerminals(v(3)) mustEqual V(2, 5)
          g.neighboursOverNonTerminals(v(4)) mustEqual V()
          g.neighboursOverNonTerminals(v(5)) mustEqual V(1, 2, 3)
          g.neighboursOverNonTerminals(v(6)) mustEqual V()
          g.neighboursOverNonTerminals(v(7)) must throwAn[AssertionError]
        }

        "over nonTerminals from multiple vertices" >> {
          g.neighboursOverNonTerminals(V(0)) mustEqual V()
          g.neighboursOverNonTerminals(V(1)) mustEqual V(5)
          g.neighboursOverNonTerminals(V(2)) mustEqual V(3, 5)
          g.neighboursOverNonTerminals(V(3)) mustEqual V(2, 5)
          g.neighboursOverNonTerminals(V(4)) mustEqual V()
          g.neighboursOverNonTerminals(V(5)) mustEqual V(1, 2, 3)
          g.neighboursOverNonTerminals(V(6)) mustEqual V()
          g.neighboursOverNonTerminals(V(7)) must throwAn[AssertionError]

          g.neighboursOverNonTerminals(V()) mustEqual V()
          g.neighboursOverNonTerminals(V(1, 2)) mustEqual V(3, 5)
          g.neighboursOverNonTerminals(V(5, 3)) mustEqual V(1, 2)
          g.neighboursOverNonTerminals(V(6, 2, 3, 4)) mustEqual V(5)
          g.neighboursOverNonTerminals(g.vertices) mustEqual V()

          val g2 = graph(V(1, 2, 3), nts = List(nt(0, (1, 2)), nt(1, (2, 1))))
          g2.neighboursOverNonTerminals(V(1, 2)) mustEqual V()
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
          g.incidentNonTerminals(v(1)) mustEqual List(nt(2, (1)), nt(3, (1, 5)), nt(3, (1, 5)))
          g.incidentNonTerminals(v(2)) mustEqual List(nt(4, (2, 3, 5)))
          g.incidentNonTerminals(v(3)) mustEqual List(nt(4, (2, 3, 5)))
          g.incidentNonTerminals(v(4)) mustEqual List()
          g.incidentNonTerminals(v(5)) mustEqual List(nt(3, (1, 5)), nt(3, (1, 5)), nt(4, (2, 3, 5)))
          g.incidentNonTerminals(v(6)) mustEqual List()
          g.incidentNonTerminals(v(7)) must throwAn[AssertionError]
        }
        "multiple vertices" >> {
          g.incidentNonTerminals(V(0)) mustEqual List()
          g.incidentNonTerminals(V(1)) mustEqual List(nt(2, (1)), nt(3, (1, 5)), nt(3, (1, 5)))
          g.incidentNonTerminals(V(2)) mustEqual List(nt(4, (2, 3, 5)))
          g.incidentNonTerminals(V(3)) mustEqual List(nt(4, (2, 3, 5)))
          g.incidentNonTerminals(V(4)) mustEqual List()
          g.incidentNonTerminals(V(5)) mustEqual List(nt(3, (1, 5)), nt(3, (1, 5)), nt(4, (2, 3, 5)))
          g.incidentNonTerminals(V(6)) mustEqual List()
          g.incidentNonTerminals(V(7)) must throwAn[AssertionError]

          g.incidentNonTerminals(V()) mustEqual E()
          g.incidentNonTerminals(V(2, 3)) mustEqual List(nt(4, (2, 3, 5)))
          g.incidentNonTerminals(V(0, 1)) mustEqual List(nt(2, (1)), nt(3, (1, 5)), nt(3, (1, 5)))
          g.incidentNonTerminals(g.vertices) mustEqual List(nt(2, (1)), nt(3, (1, 5)), nt(3, (1, 5)), nt(4, (2, 3, 5))) // without NT(1)
        }
      }

      "induced" >> {
        "edges" >> {
          g.inducedEdges(V(0)) mustEqual E()
          g.inducedEdges(V(1)) mustEqual E()
          g.inducedEdges(V(2)) mustEqual E()
          g.inducedEdges(V(3)) mustEqual E()
          g.inducedEdges(V(4)) mustEqual E()
          g.inducedEdges(V(5)) mustEqual E()
          g.inducedEdges(V(6)) mustEqual E()
          g.inducedEdges(V(7)) must throwAn[AssertionError]

          g.inducedEdges(V()) mustEqual E()
          g.inducedEdges(V(2, 3)) mustEqual E(2 -> 3)
          g.inducedEdges(V(0, 1)) mustEqual E(1 -> 0)
          g.inducedEdges(V(0, 1, 2, 6)) mustEqual E(1 -> 0, 1 -> 2)
          g.inducedEdges(V(5, 3)) mustEqual E(5 -> 3, 3 -> 5)
          g.inducedEdges(g.vertices) mustEqual g.edges
        }
        "nonterminals" >> {
          g.inducedNonTerminals(V(0)) must containTheSameElementsAs(List(nt(1)))
          g.inducedNonTerminals(V(1)) must containTheSameElementsAs(List(nt(1), nt(2, (1))))
          g.inducedNonTerminals(V(2)) must containTheSameElementsAs(List(nt(1)))
          g.inducedNonTerminals(V(3)) must containTheSameElementsAs(List(nt(1)))
          g.inducedNonTerminals(V(4)) must containTheSameElementsAs(List(nt(1)))
          g.inducedNonTerminals(V(5)) must containTheSameElementsAs(List(nt(1)))
          g.inducedNonTerminals(V(6)) must containTheSameElementsAs(List(nt(1)))
          g.inducedNonTerminals(V(7)) must throwAn[AssertionError]

          g.inducedNonTerminals(V()) must containTheSameElementsAs(List(nt(1)))
          g.inducedNonTerminals(V(2, 3)) must containTheSameElementsAs(List(nt(1)))
          g.inducedNonTerminals(V(0, 1)) must containTheSameElementsAs(List(nt(1), nt(2, (1))))
          g.inducedNonTerminals(V(1, 5)) must containTheSameElementsAs(List(nt(1), nt(2, (1)), nt (3, (1, 5)), nt(3, (1, 5))))
          g.inducedNonTerminals(V(2, 1, 5, 3)) must containTheSameElementsAs(List(nt(1), nt(2, (1)), nt(3, (1, 5)), nt(3, (1, 5)), nt(4, (2, 3, 5))))
          g.inducedNonTerminals(g.vertices) must containTheSameElementsAs(g.nonTerminals)
        }
        "subgraph assert" >> {
          g.inducedSubGraph(V(10 to 12)) must throwAn[AssertionError]
        }
        "subgraph" >> {
          g.inducedSubGraph(V()) mustEqual graph(nt(1))

          val sub = g.inducedSubGraph(V(5, 3, 2))
          sub.vertices mustEqual V(5, 3, 2)
          sub.edges mustEqual E(2 -> 3, 3 -> 5, 5 -> 3)
          sub.nonTerminals must containTheSameElementsAs(List(nt(1), nt(4, (2, 3, 5))))

          g.inducedSubGraph(g.vertices) mustEqual g
        }
      }
    }

    "metrics" >> {
      val g = graph(V(0 to 6), E(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3))
      "inDegree" >> {
        g.inDegree(v(0)) mustEqual 1
        g.inDegree(v(1)) mustEqual 0
        g.inDegree(v(2)) mustEqual 1
        g.inDegree(v(3)) mustEqual 2
        g.inDegree(v(4)) mustEqual 1
        g.inDegree(v(5)) mustEqual 1
        g.inDegree(v(6)) mustEqual 0
        g.inDegree(v(7)) must throwAn[AssertionError]
      }
      "outDegree" >> {
        g.outDegree(v(0)) mustEqual 0
        g.outDegree(v(1)) mustEqual 2
        g.outDegree(v(2)) mustEqual 2
        g.outDegree(v(3)) mustEqual 1
        g.outDegree(v(4)) mustEqual 0
        g.outDegree(v(5)) mustEqual 1
        g.outDegree(v(6)) mustEqual 0
        g.outDegree(v(7)) must throwAn[AssertionError]
      }
      "degree" >> {
        g.degree(v(0)) mustEqual 1
        g.degree(v(1)) mustEqual 2
        g.degree(v(2)) mustEqual 3
        g.degree(v(3)) mustEqual 3
        g.degree(v(4)) mustEqual 1
        g.degree(v(5)) mustEqual 2
        g.degree(v(6)) mustEqual 0
        g.degree(v(7)) must throwAn[AssertionError]
      }

      "isComplete" >> {
        graph(V(), E()).isComplete must beTrue
        graph(V(1), E()).isComplete must beTrue
        graph(V(1, 2), E()).isComplete must beFalse
        graph(V(1, 2), E(1 -> 2)).isComplete must beFalse
        graph(V(1, 2), E(1 -> 2, 2 -> 1)).isComplete must beTrue
      }

      "isEmpty" >> {
        graph(V(), E()).isEmpty must beTrue
        graph(V(1), E()).isEmpty must beFalse
        graph(V(1, 2), E()).isEmpty must beFalse
        graph(V(1, 2), E(1 -> 2)).isEmpty must beFalse
      }

      "isConnected" >> {
        graph(V(), E()).isConnected must beTrue
        graph(V(1), E()).isConnected must beTrue
        graph(V(1, 2), E()).isConnected must beFalse
        graph(V(1, 2), E(1 -> 2)).isConnected must beTrue
        graph(V(1, 2, 3), E(1 -> 2)).isConnected must beFalse
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
          graph(V(0 to 2), nts = List(nt(1, (1, 2)))) + nt(1, (1, 2)) mustEqual graph(V(0 to 2), nts = List(nt(1, (1, 2)), nt(1, (1, 2))))
        }
        "nonexisting" >> {
          graph(V(0 to 2), nts = List(nt(1, (1, 2)))) + nt(2, (0, 2)) mustEqual graph(V(0 to 2), nts = List(nt(2, (0, 2)), nt(1, (1, 2))))
        }
      }

      val g = graph(
        V(0 to 4),
        E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2),
        vData(3 -> "a", 4 -> "b"),
        eData((1 -> 3) -> 17L, (0 -> 3) -> -15L, 0 -> 4 -> 18L),
        List(nt(1, (0, 2)), nt(2, (3, 2))),
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
            vData(3 -> "a"),
            eData((1 -> 3) -> 17L, (0 -> 3) -> -15L),
            List(nt(1, (0, 2)), nt(2, (3, 2))), c = C(0, 1, 2)
          )
        }

        "existing vertex with edgedata and nonterminals" >> {
          g - v(3) mustEqual graph(
            V(0, 1, 2, 4),
            E(0 -> 4, 4 -> 2),
            vData(4 -> "b"),
            eData(0 -> 4 -> 18L),
            nts = List(nt(1, (0, 2))), c = C(0, 1, 2)
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
            vData(3 -> "a", 4 -> "b"),
            eData((1 -> 3) -> 17L, (0 -> 3) -> -15L, 0 -> 4 -> 18L),
            nts = List(nt(1, (0, 2)), nt(2, (3, 2))), c = C(0, 1, 2)
          )
        }

        "existing edge with data" >> {
          g - e(0 -> 3) mustEqual graph(
            V(0 to 4),
            E(1 -> 3, 0 -> 4, 4 -> 2),
            vData(3 -> "a", 4 -> "b"),
            eData((1 -> 3) -> 17L, 0 -> 4 -> 18L),
            List(nt(1, (0, 2)), nt(2, (3, 2))), c = C(0, 1, 2)
          )
        }
      }

      "remove nonterminal" >> {
        "from empty graph" >> {
          graph() - nt(1) must throwA[AssertionError]
        }

        "existing nonterminal" >> {
          g - nt(1, (0, 2)) mustEqual graph(
            V(0 to 4),
            E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2),
            vData(3 -> "a", 4 -> "b"),
            eData((1 -> 3) -> 17L, (0 -> 3) -> -15L, 0 -> 4 -> 18L),
            List(nt(2, (3, 2))), c = C(0, 1, 2)
          )
        }

        "multiple nonterminals with same label" >> {
          val g = graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2), nts = List(nt(1, (0, 2)), nt(1, (3, 2))), c = C(0, 1, 2))
          g - nt(1, (3, 2)) mustEqual graph(V(0 to 4), E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2), nts = List(nt(1, (0, 2))), c = C(0, 1, 2))
        }

        "multiple nonterminals with same label connected to same vertices" >> {
          val g = graph(V(0 to 2), nts = List(nt(1, (0, 2, 1)), nt(1, (0, 2, 1))), c = C(0, 1))
          g - nt(1, (0, 2, 1)) mustEqual graph(V(0 to 2), nts = List(nt(1, (0, 2, 1))), c = C(0, 1))
        }

        "nonexisting nonterminal" >> {
          g - nt(3) must throwA[AssertionError]
        }
      }

      "merge graphs" >> {
        "assert" >> {
          g ++ graph(V(0), c = C(0)) must throwAn[AssertionError]
        }
        "example" >> {
          val g = graph(
            V(0 to 2),
            E(0 -> 1, 1 -> 2),
            vData(0 -> 0, 2 -> 0),
            eData((0 -> 1) -> "A", (1 -> 2) -> "A"),
            List(nt(1, (0, 1))),
            c = C(1)
          )
          val g2 = graph(
            V(1 to 3),
            E(2 -> 1, 3 -> 2, 1 -> 2),
            vData(2 -> 1, 3 -> 1),
            eData((1 -> 2) -> "B"),
            List(nt(2))
          )
          g ++ g2 mustEqual graph(V(0 to 3), E(0 -> 1, 1 -> 2, 2 -> 1, 3 -> 2), vData(0 -> 0, 2 -> 1, 3 -> 1), eData((0 -> 1) -> "A", (1 -> 2) -> "B"), List(nt(1, (0, 1)), nt(2)), c = C(1))
        }
      }

      "remove subgraph" >> {
        "assertions" >> {
          g -- graph(V(0 to 17)) must throwAn[AssertionError]
          g -- cgraph(C(2), V(0 to 1)) must throwAn[AssertionError]
        }
        "full example" >> {
          val g = graph(
            V(0 to 4),
            E(0 -> 3, 1 -> 3, 0 -> 4, 4 -> 2),
            Map.empty,
            Map.empty,
            List(nt(1, (0, 4)), nt(2, (3, 2)), nt(3)),
            c = C(0, 1, 2)
          )
          g -- graph(V(1 to 3), nts = List(nt(3))) mustEqual graph(V(0, 4), E(0 -> 4), nts = List(nt(1, (0, 4))), c = C(0))
        }
        "with data" >> {
          val g = graph(
            V(0 to 4),
            E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4),
            vData(0 -> 1, 4 -> 2),
            eData((0 -> 1) -> "A", (3 -> 4) -> "B"),
            List(nt(1, (0, 1)), nt(2, (0, 4))),
            c = C(1)
          )
          g -- graph(V(2 to 4)) mustEqual graph(V(0 to 1), E(0 -> 1), vData(0 -> 1), eData((0 -> 1) -> "A"), List(nt(1, (0, 1))), C(1))
        }
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
        g.depthFirstSearch(v(0), v => g.successors(v).toList.sortBy(-_.label)).toList mustEqual VL(0, 1, 3, 2)
      }
      "undirected cycle" >> {
        val g = graph(V(0 to 3), E(0 -> 1, 0 -> 2, 1 -> 3, 3 -> 2))
        g.depthFirstSearch(v(0), v => g.successors(v).toList.sortBy(-_.label)).toList mustEqual VL(0, 1, 3, 2)
        g.depthFirstSearch(v(0), v => g.successors(v).toList.sortBy(_.label)).toList mustEqual VL(0, 2, 1, 3)
      }
    }

    "connected components" >> {
      graph(V(0, 1, 2), E(0 -> 1, 1 -> 2)).connectedComponents mustEqual Set(V(0, 1, 2))
      graph(V(0, 1, 2), E(0 -> 1)).connectedComponents mustEqual Set(V(0, 1), V(2))
      graph(V(0 to 5), E(1 -> 2, 2 -> 1, 3 -> 5, 4 -> 5)).connectedComponents mustEqual Set(V(0), V(1, 2), V(3, 4, 5))
    }

    "graph isomorphism" >> {
      (graph(V(1)) isIsomorphicTo graph(V())) must beFalse
      (graph(V(1)) isIsomorphicTo graph(V(2))) must beTrue
      (graph(V(1, 3)) isIsomorphicTo graph(V(2))) must beFalse
      (graph(V(1, 3)) isIsomorphicTo graph(V(2, 4))) must beTrue
      (graph(V(1, 3), E(1 -> 3)) isIsomorphicTo graph(V(2, 4))) must beFalse
      (graph(V(1, 3), E(1 -> 3)) isIsomorphicTo graph(V(2, 4), E(4 -> 2))) must beTrue
      (graph(V(1, 3), E(1 -> 3)) isIsomorphicTo graph(V(2, 4), E(4 -> 2, 2 -> 4))) must beFalse
      (graph(V(1, 3), E(1 -> 3, 3 -> 1)) isIsomorphicTo graph(V(2, 4), E(4 -> 2, 2 -> 4))) must beTrue
      (graph(V(0 to 4), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4)) isIsomorphicTo graph(V(0 to 4), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4))) must beTrue
      (graph(V(0 to 5), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 1 -> 5)) isIsomorphicTo graph(V(0 to 5), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 2 -> 5))) must beFalse
      (graph(V(0 to 5), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 1 -> 0)) isIsomorphicTo graph(V(0 to 5), E(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 2 -> 0))) must beFalse
    }

    "replace nonTerminal by graph with connectors" >> {
      "assert: must replace existing nonTerminal" >> {
        graph(V(), nts = List(nt(1))).replaceOne(nt(2), graph(), AutoId(0)) must throwAn[AssertionError]
        graph(V(1), nts = List(nt(1, (1)))).replaceOne(nt(1, (1)), graph(V(1), c = C()), AutoId(0)) must throwAn[AssertionError]
      }

      "plain" >> {
        val g = graph(V(1 to 6), E(1 -> 2, 3 -> 1, 4 -> 5, 5 -> 6), nts = List(nt(2, (2, 3, 4, 5))))
        val r = cgraph(C(1, 2, 3, 4), V(1 to 6), E(1 -> 6, 6 -> 2, 2 -> 5, 5 -> 6, 6 -> 3, 5 -> 3, 6 -> 4))
        val result = graph(V(1 to 8), E(1 -> 2, 2 -> 7, 3 -> 1, 3 -> 8, 4 -> 5, 5 -> 6, 7 -> 3, 7 -> 4, 7 -> 5, 8 -> 4, 8 -> 7))
        g.replaceOne(nt(2, (2, 3, 4, 5)), r, AutoId(7)) mustEqual result
      }

      "with data, nonterminals and connectors" >> {
        val g = graph(V(1 to 6), E(1 -> 2, 3 -> 1, 4 -> 5, 5 -> 6),
          vd = vData(3 -> "a"),
          ed = eData((5 -> 6) -> "x"),
          nts = List(nt(2, (2, 3, 4, 5)), nt(2, (2, 3, 5, 6))),
          c = C(1, 2))
        val r = cgraph(C(1, 2, 3, 4), V(1 to 6), E(1 -> 6, 6 -> 2, 2 -> 5, 5 -> 6, 6 -> 3, 5 -> 3, 6 -> 4),
          vd = vData(6 -> "b"),
          ed = eData((6 -> 3) -> "y"),
          nts = List(nt(2, (1, 5, 3, 4))) //nonTerminal with label 2 (same as in original graph)
        )
        val result = graph(V(1 to 8), E(1 -> 2, 2 -> 7, 3 -> 1, 3 -> 8, 4 -> 5, 5 -> 6, 7 -> 3, 7 -> 4, 7 -> 5, 8 -> 4, 8 -> 7),
          vd = vData(3 -> "a", 7 -> "b"),
          ed = eData((5 -> 6) -> "x", (7 -> 4) -> "y"),
          nts = List(nt(2, (2, 3, 5, 6)), nt(2, (2, 8, 4, 5))),
          c = C(1, 2))
        g.replaceOne(nt(2, (2, 3, 4, 5)), r, AutoId(7)) mustEqual result
      }
      "with duplicate nonterminal" >> {
        val g = graph(V(0 to 1), E(), nts = List(nt(1, (0, 1)), nt(1, (0, 1))))
        val r = cgraph(C(0, 1), V(0 to 2), E(0 -> 2, 2 -> 1))
        val result1 = graph(V(0 to 2), E(0 -> 2, 2 -> 1), nts = List(nt(1, (0, 1))))
        val result2 = graph(V(0 to 3), E(0 -> 2, 2 -> 1, 0 -> 3, 3 -> 1))
        g.replaceOne(nt(1, (0, 1)), r, AutoId(2)) mustEqual result1
        result1.replaceOne(nt(1, (0, 1)), r, AutoId(3)) mustEqual result2
      }
    }
  }
}
