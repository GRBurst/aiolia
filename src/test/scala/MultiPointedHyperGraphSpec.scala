package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

import aiolia.test.Helpers._

class MultiPointedHyperGraphSpec extends org.specs2.mutable.Specification {
  // TODO: move simple operations to HyperGraphSpec (only test, whats implemented in MultiPointedHyperGraph)
  "multipointedhypergraph" >> {
    "remove" >> {
      val mphg = MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0, 2)), HyperEdge(2, List(3, 2)))))

      "vertex" >> {
        "from empty multipointedhypergraph" >> {
          MultiPointedHyperGraph() - 1 must throwA[AssertionError]
        }

        "existing vertex with edges" >> {
          mphg - 4 mustEqual MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(3, edges = Set(0 -> 3, 1 -> 3), hyperEdges = List(HyperEdge(1, List(0, 2)), HyperEdge(2, List(3, 2)))))
        }
        "existing vertex with hyperedges" >> {
          mphg - 3 mustEqual MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(Set(0,1,2,4), edges = Set(0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0, 2)))))
        }
        "nonexisting vertex" >> {
          mphg - 17 must throwA[AssertionError]
        }
        "input vertex" >> {
          mphg - 0 must throwA[AssertionError]
        }
        "ouput vertex" >> {
          mphg - 2 must throwA[AssertionError]
        }
      }

      "edge" >> {
        "from empty multipointedhypergraph" >> {
          MultiPointedHyperGraph() - (1 -> 2) must throwA[AssertionError]
        }

        "existing edge" >> {
          mphg - (4 -> 2) mustEqual MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4), hyperEdges = List(HyperEdge(1, List(0, 2)), HyperEdge(2, List(3, 2)))))
        }

        "nonexisting edge" >> {
          mphg - (2 -> 4) must throwA[AssertionError]
        }
      }

      "hyperedge" >> {
        "from empty multipointedhypergraph" >> {
          MultiPointedHyperGraph() - HyperEdge(1) must throwA[AssertionError]
        }

        "existing hyperedge" >> {
          mphg - HyperEdge(1, List(0, 2)) mustEqual MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(2, List(3, 2)))))
        }

        "multiple hyperedges with same label" >> {
          val mphg = MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0, 2)), HyperEdge(1, List(3, 2)))))
          mphg - HyperEdge(1, List(3, 2)) mustEqual MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0, 2)))))
        }

        "multiple hyperedges with same label connected to same vertices" >> {
          val mphg = MultiPointedHyperGraph(connectors = List(0, 1), HyperGraph(2, hyperEdges = List(HyperEdge(1, List(0,2, 1)), HyperEdge(1, List(0,2, 1)))))
          mphg - HyperEdge(1, List(0,2, 1)) mustEqual MultiPointedHyperGraph(connectors = List(0, 1), HyperGraph(2, hyperEdges = List(HyperEdge(1, List(0,2, 1)))))
        }

        "nonexisting hyperedge" >> {
          mphg - HyperEdge(3) must throwA[AssertionError]
        }
      }
    }
  }
}
