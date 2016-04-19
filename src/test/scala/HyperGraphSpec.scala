package aiolia.hypergraph

import aiolia.test.Helpers._
import aiolia.graph._

class HyperGraphSpec extends org.specs2.mutable.Specification {
  "hyperedge" >> {
    "contains" >> {
      HyperEdge(1, List(1,2,3)).contains(0) must beFalse
      HyperEdge(1, List(1,2,3)).contains(1) must beTrue
      HyperEdge(1, List(1,2,3)).contains(2) must beTrue
      HyperEdge(1, List(1,2,3)).contains(3) must beTrue
      HyperEdge(1, List(1,2,3)).contains(4) must beFalse
    }
    "toString" >> {
      HyperEdge(2, List(1,7,3,4)).toString mustEqual "[2:1-7-3-4]"
    }
  }
  "hypergraph" >> {
    "assertions" >> {
      "vertex data" >> { HyperGraph(vertexData = vertexData(1 -> 5)) must throwAn[AssertionError] }
      "edge data" >> { HyperGraph(edgeData = edgeData((1->2) -> 5)) must throwAn[AssertionError] }
      "edges" >> {
        HyperGraph(vertices = Set(1), edges = Set(1 -> 2)) must throwAn[AssertionError]
        HyperGraph(vertices = Set(2), edges = Set(1 -> 2)) must throwAn[AssertionError]
      }
      "hyperEdges" >> {
        HyperGraph(vertices = Set(1), hyperEdges = List(HyperEdge(1, List(1, 2, 3)))) must throwAn[AssertionError]
      }
    }
    "modifications" >> {
      "remove vertex" >> {
        "nonexisting vertex" >> {
          (HyperGraph(2) - Vertex(18)) must throwAn[AssertionError]
        }
        "existing vertex" >> {
          val g = HyperGraph(2,
            hyperEdges = List(HyperEdge(1, List(0, 2, 1)), HyperEdge(2, List(2, 0))),
            edges = Set(0 -> 1, 1 -> 2, 0 -> 2)
            )
          val wanted = HyperGraph(
            vertices = Set(0,2),
            hyperEdges = List(HyperEdge(2, List(2, 0))),
            edges = Set(0 -> 2)
          )
          (g - Vertex(1)) mustEqual wanted
        }
        "with data" >> {
          val g = HyperGraph(2,
            edges = Set(0 -> 1, 1 -> 2, 0 -> 2),
            vertexData = vertexData(1 -> "x", 2 -> "y"),
            edgeData = edgeData((0->1) -> "a", (1->2) -> "b", (0->2) -> "c")
            )
          val wanted = HyperGraph(
            vertices = Set(0,2),
            edges = Set(0 -> 2),
            vertexData = vertexData(2 -> "y"),
            edgeData = edgeData((0->2) -> "c")
          )
          (g - Vertex(1)) mustEqual wanted
        }
      }

      "remove edge" >> {
        "nonexisting" >> {
          (HyperGraph(2) - Edge(18, 19)) must throwAn[AssertionError]
        }
        "existing edge" >> {
          (HyperGraph(2, edges = Set(0 -> 2, 0 -> 1)) - Edge(0, 1)) mustEqual HyperGraph(2, edges = Set(0 -> 2))
        }
        "with data" >> {
          val g = HyperGraph(2, edges = Set(0 -> 2, 0 -> 1), edgeData = edgeData((0->2) -> "a", (0 -> 1) -> "b"))
          (g - Edge(0, 1)) mustEqual HyperGraph(2, edges = Set(0 -> 2), edgeData = edgeData((0->2) -> "a"))
        }
      }

      "remove hyperedge" >> {
        "nonexisting" >> {
          (HyperGraph(2) - HyperEdge(18, List(19,20))) must throwAn[AssertionError]
        }
        "existing hyperedge" >> {
          (HyperGraph(2, List(HyperEdge(1, List(0,2)))) - HyperEdge(1, List(0,2))) mustEqual HyperGraph(2)
        }
      }

      "remove subgraph" >> {
        "assertions" >> todo
        "full example" >> todo
        "with data" >> todo
      }

      "add vertex" >> {
        "existing" >> {
          HyperGraph(2) + Vertex(1) must throwAn[AssertionError]
        }
        "nonexisting" >> {
          HyperGraph(2) + Vertex(3) mustEqual HyperGraph(3)
        }
      }

      "add edge" >> {
        "existing" >> {
          HyperGraph(2, edges = Set(1 -> 2)) + Edge(1,2) must throwAn[AssertionError]
        }
        "nonexisting" >> {
          HyperGraph(2, edges = Set(1 -> 2)) + Edge(0,2) mustEqual HyperGraph(2, edges = Set(1 -> 2, 0 -> 2))
        }
      }

      "add hyperedge" >> {
        "existing" >> {
          HyperGraph(2, List(HyperEdge(1, List(1, 2)))) + HyperEdge(1,List(1,2)) mustEqual HyperGraph(2, List(HyperEdge(1, List(1, 2)), HyperEdge(1, List(1, 2))))
        }
        "nonexisting" >> {
          HyperGraph(2, List(HyperEdge(1, List(1, 2)))) + HyperEdge(2,List(0,2)) mustEqual HyperGraph(2, List(HyperEdge(2,List(0,2)), HyperEdge(1, List(1, 2))))
        }
      }
    }
    "traversal" >> {
      "neighbours" >> {
        "edges" >> todo
        "hyperedges" >> todo
        "mixed"  >> todo
        "form multiple vertices" >> todo
      }

      "incident edges" >> {
        "singe vertex" >> todo
        "multiple vertices" >> todo
      }

      "incident hyperedges" >> {
        "singe vertex" >> todo
        "multiple vertices" >> todo
      }
    }

    "induced" >> {
      "edges" >> todo
      "hyperedges" >> todo
      "subgraph" >> todo
      "subgraph with data" >> todo
    }

    "convert to regular graph" >> {
      "with hyperedges" >> todo
      "without hyperedges" >> todo
      "with data" >> todo
    }
  }
}
