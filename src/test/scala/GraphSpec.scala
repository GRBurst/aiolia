package aiolia.graph

import aiolia.test.Helpers._

class GraphSpec extends org.specs2.mutable.Specification {
  "vertex" >> {
    "toString" >> {
      Vertex(2).toString mustEqual "2"
    }
  }
  "edge" >> {
    "toString" >> {
      Edge(Vertex(2), Vertex(3)).toString mustEqual "2 -> 3"
    }
    "contains vertex" >> {
      (Edge(Vertex(2), Vertex(3)) contains Vertex(1)) must beFalse
      (Edge(Vertex(2), Vertex(3)) contains Vertex(2)) must beTrue
      (Edge(Vertex(2), Vertex(3)) contains Vertex(3)) must beTrue
      (Edge(Vertex(2), Vertex(3)) contains Vertex(4)) must beFalse
    }
  }
  "graph" >> {
    "assertions" >> {
      "vertex data" >> { Graph(vertexData = vertexData(1 -> 5)) must throwAn[AssertionError] }
      "edge data" >> { Graph(vertices = Set(1,2), edgeData = edgeData((1->2) -> 5)) must throwAn[AssertionError] }
      "edges" >> {
        Graph(vertices = Set(1), edges = Set(1 -> 2)) must throwAn[AssertionError]
        Graph(vertices = Set(2), edges = Set(1 -> 2)) must throwAn[AssertionError]
      }
    }
    "toString" >> {
      "simple" >> {
        Graph(5, Set(1 -> 0, 1 -> 2, 2 -> 4, 2 -> 3, 3 -> 5, 5 -> 3)).toString mustEqual "Graph([0 1 2 3 4 5], [1 -> 0, 1 -> 2, 2 -> 3, 2 -> 4, 3 -> 5, 5 -> 3])"
      }
      "with vertexData" >> {
        Graph(1, Set(1 -> 0), vertexData(0 -> "wurst", 1 -> "katapult")
          ).toString mustEqual "Graph([0 1], [1 -> 0], {0: wurst, 1: katapult})"
      }
      "with edgeData" >> {
        Graph(1, Set(1 -> 0, 0 -> 1), edgeData = edgeData((1->0) -> "kanone", (0->1) -> "salat")
          ).toString mustEqual "Graph([0 1], [0 -> 1, 1 -> 0], {0->1: salat, 1->0: kanone})"
      }
      "with both data" >> {
        Graph(1, Set(1 -> 0, 0 -> 1),
          vertexData(0 -> "wurst", 1 -> "katapult"),
          edgeData((1->0) -> "kanone", (0->1) -> "salat")
          ).toString mustEqual "Graph([0 1], [0 -> 1, 1 -> 0], {0: wurst, 1: katapult}, {0->1: salat, 1->0: kanone})"
      }
    }
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
    "depth first search" >> {
      "nonexistent vertex" >> {
        Graph().depthFirstSearch(17).toList must throwAn[AssertionError]
      }
      "one vertex" >> {
        val graph = Graph(0)
        graph.depthFirstSearch(0).toList mustEqual vertexList(0)
      }
      "directed cycle" >> {
        val graph = Graph(3, Set(0 -> 1, 0 -> 2, 1 -> 3, 3 -> 2, 3 -> 0, 2 -> 1))
        graph.depthFirstSearch(0, revSort = _.toList.sortBy(-_.label)).toList mustEqual vertexList(0, 1, 3, 2)
      }
      "undirected cycle" >> {
        val graph = Graph(3, Set(0 -> 1, 0 -> 2, 1 -> 3, 3 -> 2))
        graph.depthFirstSearch(0, revSort = _.toList.sortBy(-_.label)).toList mustEqual vertexList(0, 1, 3, 2)
        graph.depthFirstSearch(0, revSort = _.toList.sortBy(_.label)).toList mustEqual vertexList(0, 2, 1, 3)
      }
    }
  }
}
