package aiolia.graph

import aiolia.test.Helpers._

class GraphSpec extends org.specs2.mutable.Specification {
  "graph should" >> {
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
