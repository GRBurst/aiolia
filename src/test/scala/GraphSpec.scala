package aiolia.graph

import aiolia.test.Helpers._

class GraphSpec extends org.specs2.mutable.Specification {
  "graph should" >> {
    "traversal accessors" >> {
      val graph = Graph(5, Set(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3))
      "successors" >> {
        graph.successors(0) mustEqual vertexSet()
        graph.successors(1) mustEqual vertexSet(0, 2)
        graph.successors(2) mustEqual vertexSet(4, 3)
        graph.successors(3) mustEqual vertexSet(5)
        graph.successors(4) mustEqual vertexSet()
        graph.successors(5) mustEqual vertexSet(3)
      }

      "predecessors" >> {
        graph.predecessors(0) mustEqual vertexSet(1)
        graph.predecessors(1) mustEqual vertexSet()
        graph.predecessors(2) mustEqual vertexSet(1)
        graph.predecessors(3) mustEqual vertexSet(2, 5)
        graph.predecessors(4) mustEqual vertexSet(2)
        graph.predecessors(5) mustEqual vertexSet(3)
      }

      "incoming edges" >> {
        graph.incomingEdges(0) mustEqual edgeSet(1 -> 0)
        graph.incomingEdges(1) mustEqual edgeSet()
        graph.incomingEdges(2) mustEqual edgeSet(1 -> 2)
        graph.incomingEdges(3) mustEqual edgeSet(2 -> 3, 5 -> 3)
        graph.incomingEdges(4) mustEqual edgeSet(2 -> 4)
        graph.incomingEdges(5) mustEqual edgeSet(3 -> 5)
      }

      "outgoing edges" >> {
        graph.outgoingEdges(0) mustEqual edgeSet()
        graph.outgoingEdges(1) mustEqual edgeSet(1 -> 0, 1 -> 2)
        graph.outgoingEdges(2) mustEqual edgeSet(2 -> 4, 2 -> 3)
        graph.outgoingEdges(3) mustEqual edgeSet(3 -> 5)
        graph.outgoingEdges(4) mustEqual edgeSet()
        graph.outgoingEdges(5) mustEqual edgeSet(5 -> 3)
      }
    }
    "detect cycle" >> {
      "no cycle" >> {
        val graph = Graph(5, Set(0 -> 1, 0 -> 2, 1 -> 3, 2 -> 4, 2 -> 5))
        graph.hasCycle must beFalse
      }
      "undirected cycle" >> {
        val graph = Graph(4, Set(0 -> 1, 2 -> 1, 2 -> 3, 3 -> 4, 4 -> 1))
        graph.hasCycle must beFalse
      }
      "directed cycle" >> {
        val graph = Graph(4, Set(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 1))
        graph.hasCycle must beTrue
      }
      "small graph cycle" >> {
        val graph = Graph(5, Set(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3))
        graph.hasCycle must beTrue
      }
    }
  }
}
