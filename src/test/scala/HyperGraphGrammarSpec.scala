package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

import aiolia.test.Helpers._

class HyperGraphGrammarSpec extends org.specs2.mutable.Specification {
  "graph grammar" >> {
    "expand deterministic" >> {
      val h1 = HyperEdge(1, List(0), List(1, 2))
      val h2 = HyperEdge(1, List(0), List(2, 1))
      val axiom = HyperGraph(2, List(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0), out = List(1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1)))
      ))

      val e1 = g.expand
      val e2 = g.expand
      e1 mustEqual e2
    }

    "expand edges" >> {
      val h1 = HyperEdge(1, List(0), List(1, 2))
      val h2 = HyperEdge(1, List(0), List(2, 1))
      val axiom = HyperGraph(2, List(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0), out = List(1, 2), HyperGraph(4, edges = Set(0 -> 2, 2 -> 3, 3 -> 4, 2 -> 4, 4 -> 1)))
      ))

      g.expand mustEqual Graph(6, Set(2 -> 4, 2 -> 3, 1 -> 5, 4 -> 3, 1 -> 6, 6 -> 5, 0 -> 1, 5 -> 2, 0 -> 2, 3 -> 1))
    }

    "expand hyperedges" >> {
      val h1 = HyperEdge(1, List(0, 1), List(2, 3))
      val axiom = HyperGraph(3, hyperEdges = List(h1))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0, 1), out = List(2, 3), HyperGraph(3, edges = Set(0 -> 1, 2 -> 3), hyperEdges = List(HyperEdge(2, List(0, 1), List(2))))),
        2 -> MultiPointedHyperGraph(in = List(0, 1), out = List(2), HyperGraph(2, edges = Set(0 -> 2, 1 -> 2)))
      ))

      g.expand mustEqual Graph(3, Set(0 -> 1, 0 -> 2, 1 -> 2, 2 -> 3))
    }

    "empty hyperedge axiom" >> {
      val g = Grammar(1, Map(
        1 -> MultiPointedHyperGraph(in = List(), out = List(), HyperGraph(2, edges = Set(0 -> 1, 1 -> 2, 2 -> 1)))
      ))

      g.expand mustEqual Graph(2, Set(0 -> 1, 1 -> 2, 2 -> 1))
    }

    "redundant replacements" >> {
      val h1 = HyperEdge(1, List(0), List(1))
      val h2 = HyperEdge(1, List(0), List(1))
      val axiom = HyperGraph(1, hyperEdges = List(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(1, edges = Set(0 -> 1, 1 -> 0)))
      ))

      g.expand mustEqual Graph(1, Set(0 -> 1, 1 -> 0))
    }

    "run in circles" >> {
      val g = Grammar(1, Map(
        1 -> MultiPointedHyperGraph(in = Nil, out = Nil, HyperGraph(1, hyperEdges = List(HyperEdge(2, List(0), List(1))))),
        2 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(1, hyperEdges = List(HyperEdge(3, List(0), List(1)), HyperEdge(3, List(1), List(0))))),
        3 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, hyperEdges = List(HyperEdge(4, List(0), List(2)), HyperEdge(4, List(2), List(1))))),
        4 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, edges = Set(0 -> 2, 2 -> 1)))
      ))

      g.expand mustEqual Graph(7, Set(4 -> 2, 5 -> 1, 6 -> 3, 1 -> 6, 3 -> 7, 7 -> 0, 0 -> 4, 2 -> 5))
    }

    "redundant hyperedge" >> {
      val g = Grammar(1, Map(
        1 -> MultiPointedHyperGraph(in = Nil, out = Nil, HyperGraph(1, hyperEdges = List(HyperEdge(2, List(0), List(1))))),
        2 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(1, hyperEdges = List(HyperEdge(3, List(0), List(1)), HyperEdge(3, List(0), List(1))))),
        3 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, edges = Set(0 -> 2, 2 -> 1)))
      ))

      g.expand mustEqual Graph(3, Set(0 -> 2, 2 -> 1, 0 -> 3, 3 -> 1))
    }

    "merge vertex data" >> {
      val h1 = HyperEdge(1, List(0), List(1))
      val axiom = HyperGraph(1, hyperEdges = List(h1), vertexData = vertexData(0 -> 5, 1 -> 6))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, edges = Set(0 -> 2, 2 -> 1), vertexData = vertexData(2 -> 7)))
      ))

      g.expand mustEqual Graph(2, Set(0 -> 2, 2 -> 1), vertexData = vertexData(0 -> 5, 1 -> 6, 2 -> 7))
    }

    "merge edge data" >> {
      val h1 = HyperEdge(1, List(0), List(1))
      val axiom = HyperGraph(2, List(h1), Set(Edge(1, 2)), edgeData = edgeData((1 -> 2) -> "Wurst"))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, edges = Set(0 -> 2, 2 -> 1), edgeData = edgeData((2 -> 1) -> "Worst")))
      ))

      g.expand mustEqual Graph(3, Set(1 -> 2, 0 -> 3, 3 -> 1), edgeData = edgeData(((1 -> 2) -> "Wurst"), (3 -> 1) -> "Worst"))
    }

    "redundant vertex data" >> {
      val h1 = HyperEdge(1, List(0), List(1))
      val h2 = HyperEdge(1, List(0), List(1))
      val axiom = HyperGraph(1, hyperEdges = List(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, edges = Set(0 -> 2, 2 -> 1), vertexData = vertexData(2 -> 1)))
      ))

      g.expand mustEqual Graph(3, Set(0 -> 2, 2 -> 1, 0 -> 3, 3 -> 1), vertexData = vertexData(3 -> 1, 2 -> 1))
    }

    "redundant edge data" >> {
      val h1 = HyperEdge(1, List(0), List(1))
      val h2 = HyperEdge(1, List(0), List(1))
      val axiom = HyperGraph(1, hyperEdges = List(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(2, edges = Set(0 -> 2, 2 -> 1), edgeData = edgeData((2 -> 1) -> "Worst")))
      ))

      g.expand mustEqual Graph(3, Set(0 -> 2, 2 -> 1, 0 -> 3, 3 -> 1), edgeData = edgeData(((2 -> 1) -> "Worst"), (3 -> 1) -> "Worst"))
    }

    "multi pointed hypergraph must use all tentacles" >> {
      MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(edges = Set(0 -> 2))) must throwAn[AssertionError]
    }

    "grammar can only have rhs hyperedges that have a corresponding lhs" >> {
      "unknown hyperedge label" >> {
        Grammar(1, Map(
          1 -> MultiPointedHyperGraph(in = Nil, out = Nil, HyperGraph(hyperEdges = List(HyperEdge(15, List(0), List(1)))))
        )) must throwAn[AssertionError]
      }

      "different hyperedge signature" >> {
        Grammar(1, Map(
          1 -> MultiPointedHyperGraph(in = Nil, out = Nil, HyperGraph(2, hyperEdges = List(HyperEdge(2, List(0), List(1))))),
          2 -> MultiPointedHyperGraph(in = List(0,1), out = List(2), HyperGraph(2, edges = Set(0 -> 2, 1 -> 2)))
        )) must throwAn[AssertionError]
      }

      "signature does not need to match node ids" >> {
        Grammar(1, Map(
          1 -> MultiPointedHyperGraph(in = Nil, out = Nil, HyperGraph(2, hyperEdges = List(HyperEdge(2, List(0), List(1))))),
          2 -> MultiPointedHyperGraph(in = List(2), out = List(3), HyperGraph(Set(2, 3), edges = Set(2 -> 3)))
        )) must not(throwAn[AssertionError])
      }
    }

    // A hypergraph can only set data on its own vertices/edges and a multi
    // pointed hypergraph may not set data for its input or output vertices.
    // => A projection can only set data for its own nodes.
    // XXX: Do hypergraphs need to set the data for all vertices and edges?
    "projections only operate on local data" >> {
      "vertex data should only contain existing vertices" >> {
        HyperGraph(edges = Set(0 -> 1), vertexData = vertexData(2 -> 200)) must throwAn[AssertionError]
      }

      "edge data should only contain existing edges" >> {
        HyperGraph(edges = Set(0 -> 1), edgeData = edgeData((1 -> 2) -> 200)) must throwAn[AssertionError]
      }

      "disallow overriding data of input vertex" >> {
        MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(vertexData = vertexData(0 -> "bier"))) must throwAn[AssertionError]
      }

      "disallow overriding data of output vertex" >> {
        MultiPointedHyperGraph(in = List(0), out = List(1), HyperGraph(vertexData = vertexData(1 -> "wein"))) must throwAn[AssertionError]
      }
    }

    "axiom has to have ids 0 until |vertices|" >> {
      val axiom = HyperGraph(Set(0,2))
      Grammar(axiom) must throwAn[AssertionError]
    }

    "grammar cannot have cycles" >> {
      val rhs1 = MultiPointedHyperGraph(hyperGraph = HyperGraph(hyperEdges = List(HyperEdge(2))))
      val rhs2 = MultiPointedHyperGraph(hyperGraph = HyperGraph(hyperEdges = List(HyperEdge(1))))
      Grammar(1, Map(1 -> rhs1, 2 -> rhs2)) must throwAn[AssertionError]
    }
  }
}
