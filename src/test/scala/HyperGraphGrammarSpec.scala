package aiolia.hypergraphgrammar

import Types._

object Helpers {
  implicit def VertexTupleToEdge(tuple: (Int, Int)) = Edge(Vertex(tuple._1), Vertex(tuple._2))
  implicit def IntToVertex(i: Int) = Vertex(i)
}

import Helpers._

class HyperGraphGrammarSpec extends org.specs2.mutable.Specification {
  "Graph Grammar" >> {
    "expand deterministic" >> {
      val h1 = HyperEdge(1, List(0), List(1, 2))
      val h2 = HyperEdge(1, List(0), List(2, 1))
      val axiom = HyperGraph(List(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(1), out = List(2, 3), edges = Set(1 -> 3, 3 -> 4, 4 -> 5, 3 -> 5, 5 -> 2))
      ))

      val e1 = g.expand
      val e2 = g.expand
      e1 mustEqual e2

    }

    "expand edges" >> {
      val h1 = HyperEdge(1, List(0), List(1, 2))
      val h2 = HyperEdge(1, List(0), List(2, 1))
      val axiom = HyperGraph(List(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(10), out = List(20, 30), edges = Set(10 -> 30, 30 -> 40, 40 -> 50, 30 -> 50, 50 -> 20))
      ))

      g.expand mustEqual Graph(Set(6 -> 2, 2 -> 4, 2 -> 3, 1 -> 5, 1 -> 6, 0 -> 1, 5 -> 6, 4 -> 1, 0 -> 2, 3 -> 4))
    }

    "expand hyperedges" >> {
      val h1 = HyperEdge(1, List(0, 1), List(2, 3))
      val axiom = HyperGraph(List(h1))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0, 1), out = List(2, 3), edges = Set(0 -> 1, 2 -> 3), hyperEdges = List(HyperEdge(2, List(0, 1), List(2)))),
        2 -> MultiPointedHyperGraph(in = List(0, 1), out = List(2), edges = Set(1 -> 2))
      ))

      g.expand mustEqual Graph(Set(0 -> 1, 1 -> 2, 2 -> 3))
    }

    "empty hyperedge axiom" >> {
      val g = Grammar(1, Map(
        1 -> MultiPointedHyperGraph(in = List(), out = List(), edges = Set(0 -> 1, 1 -> 2, 2 -> 1))
      ))

      g.expand mustEqual Graph(Set(0 -> 1, 1 -> 2, 2 -> 1))
    }

    "redundant replacements" >> {
      val h1 = HyperEdge(1, List(0), List(1))
      val h2 = HyperEdge(1, List(0), List(1))
      val axiom = HyperGraph(List(h1))

      val g = Grammar(axiom, Map(
        1 -> MultiPointedHyperGraph(in = List(0), out = List(1), edges = Set(0 -> 1, 1 -> 0))
      ))

      g.expand mustEqual Graph(Set(0 -> 1, 1 -> 0))
    }

    "run in circles" >> {
      val g = Grammar(1, Map(
        1 -> MultiPointedHyperGraph(in = Nil, out = Nil, hyperEdges = List(HyperEdge(2, List(0), List(1)))),
        2 -> MultiPointedHyperGraph(in = List(1), out = List(2), hyperEdges = List(HyperEdge(3, List(1), List(2)), HyperEdge(3, List(2), List(1)))),
        3 -> MultiPointedHyperGraph(in = List(1), out = List(2), hyperEdges = List(HyperEdge(4, List(1), List(3)), HyperEdge(4, List(3), List(2)))),
        4 -> MultiPointedHyperGraph(in = List(1), out = List(2), edges = Set(1 -> 3, 3 -> 2))
      ))

      g.expand mustEqual Graph(Set(4 -> 2, 5 -> 1, 6 -> 3, 1 -> 6, 3 -> 7, 7 -> 0, 0 -> 4, 2 -> 5))
    }

    "redundant hyperedge" >> {
      val g = Grammar(1, Map(
        1 -> MultiPointedHyperGraph(in = Nil, out = Nil, hyperEdges = List(HyperEdge(2, List(0), List(1)))),
        2 -> MultiPointedHyperGraph(in = List(1), out = List(2), hyperEdges = List(HyperEdge(3, List(1), List(2)), HyperEdge(3, List(1), List(2)))),
        3 -> MultiPointedHyperGraph(in = List(1), out = List(2), edges = Set(1 -> 3, 3 -> 2))
      ))

      g.expand mustEqual Graph(Set(0 -> 2, 2 -> 1, 0 -> 3, 3 -> 1))
    }
  }
}
