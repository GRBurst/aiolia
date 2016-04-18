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
      "from grammar" >> {
        val random = mock[Random]

        val h1 = HyperEdge(1, List(0), List(1, 2))
        val h2 = HyperEdge(1, List(0), List(2, 1))
        val axiom = HyperGraph(2, List(h1, h2))

        val rhsA1 = MultiPointedHyperGraph(in = List(0), out = List(1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), hyperEdges = List(HyperEdge(2, in = List(0, 2)))))
        val rhsA2 = MultiPointedHyperGraph(in = List(0, 2), hyperGraph = HyperGraph(2, edges = Set(0 -> 1, 1 -> 2)))
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

        val h1 = HyperEdge(1, List(0), List(1, 2))
        val h2 = HyperEdge(1, List(0), List(2, 1))
        val axiom = HyperGraph(2, List(h1, h2))

        val rhsA1 = MultiPointedHyperGraph(in = List(0), out = List(1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1), hyperEdges = List(HyperEdge(2, in = List(0, 2)))))
        val rhsA2 = MultiPointedHyperGraph(in = List(0, 1), hyperGraph = HyperGraph(1, edges = Set(0 -> 1)))
        val g = Grammar(axiom, Map( 1 -> rhsA1, 2 -> rhsA2))

        random.select(g.productions) returns (2 -> rhsA2)

        Mutation.removeRandomVertex(g, random) mustEqual None
      }
    }
  }
}
