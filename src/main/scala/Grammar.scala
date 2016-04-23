package aiolia

import aiolia.helpers._
import aiolia.graph._
import aiolia.graph.types._

case class Grammar[+V, +E](axiom: Graph[V, E], productions: Map[Label, Graph[V, E]] = Map.empty[Label, Graph[V, E]]) {
  assert((0 until axiom.vertices.size).forall(axiom.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${axiom.vertices}") // needed for autoId in expand
  assert(productions.values.flatMap(_.nonTerminals).forall { nonTerminal =>
    val rhs = productions.get(nonTerminal.label)
    rhs.isDefined && (nonTerminal.connectors.size == rhs.get.connectors.size)
  }, "All nonterminals on the rhs need to have an equivalent on the lhs")
  assert(!dependencyGraph.hasCycle, "this grammer contains cycles, which it shouldn't, so shit see this instead.")
  //TODO: assert axiom must not have connectors

  def addProduction[V1 >: V, E1 >: E](production: (Label, Graph[V1, E1])): Grammar[V1, E1] = {
    assert(!(productions.keys.toSet contains production._1), s"productions already contain a rule with label ${production._1}\nproductions:\n${productions.mkString("\n")}")

    copy(productions = productions + production)
  }

  def removeProduction(nonTerminal: Label) = {
    //TODO: assert: label should not be used in rhs of all productions
    copy(productions = productions - nonTerminal)
  }

  def dependencyGraph = {
    val vertices = productions.keys.map(Vertex(_)).toSet
    val edges = productions.flatMap {
      case (lhs, rhs) =>
        rhs.nonTerminals.map(h => Edge(Vertex(lhs), Vertex(h.label)))
    }.toSet

    Graph(vertices, edges)
  }

  def cleanup: Grammar[V, E] = {
    val starts: List[Label] = axiom.nonTerminals.map(_.label).distinct
    val keep: Set[Vertex] = starts.flatMap(start => dependencyGraph.depthFirstSearch(Vertex(start))).toSet
    copy(productions = productions.filterKeys(keep contains Vertex(_)))
  }

  def expand = {
    var current = axiom
    val autoId = AutoId(axiom.vertices.size) // assuming that vertices have labels 0..n

    while (current.nonTerminals.nonEmpty) {
      val nonTerminal = current.nonTerminals.head
      val replacement = productions(nonTerminal.label)
      current = current.replaceOne(nonTerminal, replacement, autoId)
    }

    current
  }

  override def toString = s"Grammar(\n$axiom\n${productions.mkString("\n")})"
}
