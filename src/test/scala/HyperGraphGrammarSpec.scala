package aiolia.hypergraphgrammar

import Types._

object Helpers {
  implicit def VertexTemplateTupleToEdgeTemplate(tuple:(Label, Label)) = EdgeTemplate(tuple._1, tuple._2)
  implicit def VertexTupleToEdge(tuple:(Int, Int)) = Edge(Vertex(tuple._1), Vertex(tuple._2))
  implicit def IntToVertex(i:Int) = Vertex(i)
}

import Helpers._

class HyperGraphGrammarSpec extends org.specs2.mutable.Specification {
  "Graph Grammar" >> {
    "expand deterministic" >> {
      val h1 = HyperEdge(1, List(0), List(1, 2))
      val h2 = HyperEdge(1, List(0), List(2, 1))
      val axiom = HyperGraph(Set(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> HyperGraphTemplate(in = List(1), out = List(2,3), edges = Set(1 -> 3, 3 -> 4, 4 -> 5, 3 -> 5, 5 -> 2))
      ))

      val e1 = g.expand
      val e2 = g.expand
      e1 mustEqual e2

    }

    "expand edges" >> {
      val h1 = HyperEdge(1, List(0), List(1, 2))
      val h2 = HyperEdge(1, List(0), List(2, 1))
      val axiom = HyperGraph(Set(h1, h2))

      val g = Grammar(axiom, Map(
        1 -> HyperGraphTemplate(in = List(1), out = List(2,3), edges = Set(1 -> 3, 3 -> 4, 4 -> 5, 3 -> 5, 5 -> 2))
      ))

      val e1 = g.expand
      e1 mustEqual Graph(Set(2 -> 4, 2 -> 3, 1 -> 5, 4 -> 3, 1 -> 6, 6 -> 5, 0 -> 1, 5 -> 2, 0 -> 2, 3 -> 1))
    }

    "expand hyperedges" >> {
      val h1 = HyperEdge(1, List(0, 1), List(2, 3))
      val axiom = HyperGraph(Set(h1))

      val g = Grammar(axiom, Map(
        1 -> HyperGraphTemplate(in = List(0, 1), out = List(2, 3), edges = Set(0 -> 1, 2 -> 3), hyperEdges = Set(HyperEdgeTemplate(2, List(0, 1), List(2)))),
          2 -> HyperGraphTemplate(in = List(0, 1), out = List(2), edges = Set( 1 -> 2 ))
      ))

      val e1 = g.expand
      e1 mustEqual Graph(Set(0 -> 1, 1 -> 2, 2 -> 3))
    }
  }
}
