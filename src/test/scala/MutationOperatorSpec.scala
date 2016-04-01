package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

import Mutation._

import aiolia.test.Helpers._

class MutationOperatorSpec extends org.specs2.mutable.Specification {
  "graph grammar mutation" >> {
    "remove" >> {
      val mphg = MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0), List(2)), HyperEdge(2, List(3), List(2)))))

      "vertex" >> {
        "from empty multipointedhypergraph" >> {
          removeVertex(MultiPointedHyperGraph(), 1) must throwA[AssertionError]
        }

        "existing vertex with edges" >> {
          removeVertex(mphg, 4) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(3, edges = Set(0 -> 3, 1 -> 3), hyperEdges = List(HyperEdge(1, List(0), List(2)), HyperEdge(2, List(3), List(2)))))
        }
        "existing vertex with hyperedges" >> {
          removeVertex(mphg, 3) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(Set(0,1,2,4), edges = Set(0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0), List(2)))))
        }
        "nonexisting vertex" >> {
          removeVertex(mphg, 17) must throwA[AssertionError]
        }
        "input vertex" >> {
          removeVertex(mphg, 0) must throwA[AssertionError]
        }
        "ouput vertex" >> {
          removeVertex(mphg, 2) must throwA[AssertionError]
        }
      }

      "edge" >> {
        "from empty multipointedhypergraph" >> {
          removeEdge(MultiPointedHyperGraph(), 1 -> 2) must throwA[AssertionError]
        }

        "existing edge" >> {
          removeEdge(mphg, 4 -> 2) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4), hyperEdges = List(HyperEdge(1, List(0), List(2)), HyperEdge(2, List(3), List(2)))))
        }

        "nonexisting edge" >> {
          removeEdge(mphg, 2 -> 4) must throwA[AssertionError]
        }
      }

      "hyperedge" >> {
        "from empty multipointedhypergraph" >> {
          removeHyperEdge(MultiPointedHyperGraph(), HyperEdge(1)) must throwA[AssertionError]
        }

        "existing hyperedge" >> {
          removeHyperEdge(mphg, HyperEdge(1, List(0), List(2))) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(2, List(3), List(2)))))
        }

        "multiple hyperedges with same label" >> {
          val mphg = MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0), List(2)), HyperEdge(1, List(3), List(2)))))
          removeHyperEdge(mphg, HyperEdge(1, List(3), List(2))) mustEqual MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(4, edges = Set(0 -> 3,1 -> 3, 0 -> 4, 4 -> 2), hyperEdges = List(HyperEdge(1, List(0), List(2)))))
        }

        "multiple hyperedges with same label connected to same vertices" >> {
          val mphg = MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, hyperEdges = List(HyperEdge(1, List(0,2), List(1)), HyperEdge(1, List(0,2), List(1)))))
          removeHyperEdge(mphg, HyperEdge(1, List(0,2), List(1))) mustEqual MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, hyperEdges = List(HyperEdge(1, List(0,2), List(1)))))
        }

        "nonexisting hyperedge" >> {
          removeHyperEdge(mphg, HyperEdge(3)) must throwA[AssertionError]
        }
      }
    }
  }
}
