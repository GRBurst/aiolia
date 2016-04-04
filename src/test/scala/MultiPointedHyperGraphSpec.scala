package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

import aiolia.test.Helpers._

class MultiPointedHyperGraphSpec extends org.specs2.mutable.Specification {
  // TODO: move simple operations to HyperGraphSpec
  "multipointedhypergraph" >> {
    "remove" >> {
      val mphg = MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0), List(2)), HyperEdge(2, List(3), List(2)))))

      "vertex" >> {
        "from empty multipointedhypergraph" >> {
          MultiPointedHyperGraph().removeVertex(1) must throwA[AssertionError]
        }

        "existing vertex with edges" >> {
          mphg.removeVertex(4) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(3, edges = Set(0 -> 3, 1 -> 3), hyperEdges = List(HyperEdge(1, List(0), List(2)), HyperEdge(2, List(3), List(2)))))
        }
        "existing vertex with hyperedges" >> {
          mphg.removeVertex(3) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(Set(0,1,2,4), edges = Set(0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0), List(2)))))
        }
        "nonexisting vertex" >> {
          mphg.removeVertex(17) must throwA[AssertionError]
        }
        "input vertex" >> {
          mphg.removeVertex(0) must throwA[AssertionError]
        }
        "ouput vertex" >> {
          mphg.removeVertex(2) must throwA[AssertionError]
        }
      }

      "edge" >> {
        "from empty multipointedhypergraph" >> {
          MultiPointedHyperGraph().removeEdge(1 -> 2) must throwA[AssertionError]
        }

        "existing edge" >> {
          mphg.removeEdge(4 -> 2) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4), hyperEdges = List(HyperEdge(1, List(0), List(2)), HyperEdge(2, List(3), List(2)))))
        }

        "nonexisting edge" >> {
          mphg.removeEdge(2 -> 4) must throwA[AssertionError]
        }
      }

      "hyperedge" >> {
        "from empty multipointedhypergraph" >> {
          MultiPointedHyperGraph().removeHyperEdge(HyperEdge(1)) must throwA[AssertionError]
        }

        "existing hyperedge" >> {
          mphg.removeHyperEdge(HyperEdge(1, List(0), List(2))) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(2, List(3), List(2)))))
        }

        "multiple hyperedges with same label" >> {
          val mphg = MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0), List(2)), HyperEdge(1, List(3), List(2)))))
          mphg.removeHyperEdge(HyperEdge(1, List(3), List(2))) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0), List(2)))))
        }

        "multiple hyperedges with same label connected to same vertices" >> {
          val mphg = MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, hyperEdges = List(HyperEdge(1, List(0,2), List(1)), HyperEdge(1, List(0,2), List(1)))))
          mphg.removeHyperEdge(HyperEdge(1, List(0,2), List(1))) mustEqual MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, hyperEdges = List(HyperEdge(1, List(0,2), List(1)))))
        }

        "nonexisting hyperedge" >> {
          mphg.removeHyperEdge(HyperEdge(3)) must throwA[AssertionError]
        }
      }
    }
  }
}
