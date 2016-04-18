package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.hypergraph._
import aiolia.graph.types._
import aiolia.hypergraphgrammar._
import aiolia.test.Helpers._
import aiolia.helpers.Random

class MutationOperatorSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {
  "mutation operator" >> {
    "remove random vertex" >> {
      "from empty grammar" >> {
        val random = mock[Random]
        Mutation.removeRandomVertex(Grammar(1), random) mustEqual None
      }

      "from grammar" >> {
        val random = mock[Random]

        val h1 = HyperEdge(1, List(0, 1, 2))
        val h2 = HyperEdge(1, List(0, 2, 1))
        val axiom = HyperGraph(2, List(h1, h2))

        val rhsA1 = MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), hyperEdges = List(HyperEdge(2, connectors = List(0, 2)))))
        val rhsA2 = MultiPointedHyperGraph(connectors = List(0, 2), hyperGraph = HyperGraph(2, edges = Set(0 -> 1, 1 -> 2)))
        val g = Grammar(axiom, Map( 1 -> rhsA1, 2 -> rhsA2))

        random.select(g.productions) returns (2 -> rhsA2)
        random.select(Set(Vertex(1))) returns Vertex(1)

        Mutation.removeRandomVertex(g, random) mustEqual Some(Grammar(axiom, Map(
          1 -> rhsA1,
          2 -> rhsA2.copy(hyperGraph = HyperGraph(Set(0,2), edges = Set.empty))
        )))
      }

      "select rule without removable vertices" >> {
        val random = mock[Random]

        val h1 = HyperEdge(1, List(0, 1, 2))
        val h2 = HyperEdge(1, List(0, 2, 1))
        val axiom = HyperGraph(2, List(h1, h2))

        val rhsA1 = MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), hyperEdges = List(HyperEdge(2, connectors = List(0, 2)))))
        val rhsA2 = MultiPointedHyperGraph(connectors = List(0, 1), hyperGraph = HyperGraph(1, edges = Set(0 -> 1)))
        val g = Grammar(axiom, Map( 1 -> rhsA1, 2 -> rhsA2))

        random.select(g.productions) returns (2 -> rhsA2)

        Mutation.removeRandomVertex(g, random) mustEqual None
      }
    }
    "remove random edge" >> {
      "from grammar" >> {
        val random = mock[Random]

        val h1 = HyperEdge(1, List(0, 1, 2))
        val h2 = HyperEdge(1, List(0, 2, 1))
        val axiom = HyperGraph(2, List(h1, h2))

        val rhsA1 = MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), hyperEdges = List(HyperEdge(2, connectors = List(0, 2)))))
        val rhsA2 = MultiPointedHyperGraph(connectors = List(0, 2), hyperGraph = HyperGraph(2, edges = Set(0 -> 1, 1 -> 2)))
        val g = Grammar(axiom, Map( 1 -> rhsA1, 2 -> rhsA2))

        random.select(g.productions) returns (1 -> rhsA1)
        random.select(edgeSet((2) -> (4), (2) -> (3), (4) -> (1), (0) -> (2), (3) -> (4))) returns Edge(2, 4)

        Mutation.removeRandomEdge(g, random) mustEqual Some(Grammar(axiom, Map(
          1 -> rhsA1.copy(hyperGraph = rhsA1.hyperGraph.copy(edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 4 -> 1))),
          2 -> rhsA2
        )))
      }

      "from empty grammar" >> {
        val random = mock[Random]
        Mutation.removeRandomEdge(Grammar(1), random) mustEqual None
      }

      "select rule without removable edges" >> {
        val random = mock[Random]

        val h1 = HyperEdge(1, List(0, 1, 2))
        val h2 = HyperEdge(1, List(0, 2, 1))
        val axiom = HyperGraph(2, List(h1, h2))

        val rhsA1 = MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), hyperEdges = List(HyperEdge(2, connectors = List(0, 2)))))
        val rhsA2 = MultiPointedHyperGraph(connectors = List(0, 1), hyperGraph = HyperGraph(1))
        val g = Grammar(axiom, Map( 1 -> rhsA1, 2 -> rhsA2))

        random.select(g.productions) returns (2 -> rhsA2)

        Mutation.removeRandomVertex(g, random) mustEqual None
      }
    }

    "inline random hyperedge" >> {
      "on empty grammar" >> {
        val random = mock[Random]
        Mutation.inlineRandomHyperEdge(Grammar(1), random) mustEqual None
      }

      "on grammar" >> {
        val random = mock[Random]

        val h1 = HyperEdge(1, List(0, 1, 2))
        val h2 = HyperEdge(1, List(0, 2, 1))
        val axiom = HyperGraph(2, List(h1, h2))

        val rhsA1 = MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), hyperEdges = List(HyperEdge(2, connectors = List(0, 2)))))
        val rhsA2 = MultiPointedHyperGraph(connectors = List(0, 2), hyperGraph = HyperGraph(2, edges = Set(0 -> 1, 1 -> 2)))
        val g = Grammar(axiom, Map( 1 -> rhsA1, 2 -> rhsA2))

        random.select(g.productions) returns (1 -> rhsA1)
        random.select(List(HyperEdge(2,List((0), (2))))) returns HyperEdge(2,List((0), (2)))

        Mutation.inlineRandomHyperEdge(g, random) mustEqual Some(Grammar(axiom, Map(
          1 -> rhsA1.copy(hyperGraph = HyperGraph(5, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1, 0 -> 5, 5 -> 2))),
          2 -> rhsA2
        )))
      }

      "on grammar rule without hyperEdge" >> {
        val random = mock[Random]

        val h1 = HyperEdge(1, List(0, 1, 2))
        val h2 = HyperEdge(1, List(0, 2, 1))
        val axiom = HyperGraph(2, List(h1, h2))

        val rhsA1 = MultiPointedHyperGraph(connectors = List(0, 1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), hyperEdges = List(HyperEdge(2, connectors = List(0, 2)))))
        val rhsA2 = MultiPointedHyperGraph(connectors = List(0, 2), hyperGraph = HyperGraph(2, edges = Set(0 -> 1, 1 -> 2)))
        val g = Grammar(axiom, Map( 1 -> rhsA1, 2 -> rhsA2))

        random.select(g.productions) returns (2 -> rhsA2)

        Mutation.inlineRandomHyperEdge(g, random) mustEqual None
      }

    }
  }
}
