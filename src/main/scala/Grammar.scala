package aiolia

import aiolia.helpers._
import aiolia.graph._
import aiolia.graph.types._

import collection.mutable
import annotation.tailrec

object Grammar {
  def minimal[V, E] = Grammar[V, E](Graph(nonTerminals = List(NonTerminal(1))), Map(1 -> Graph()))
}

case class Grammar[+V, +E](axiom: Graph[V, E], productions: Map[Label, Graph[V, E]] = Map.empty[Label, Graph[V, E]]) {
  assert((0 until axiom.vertices.size).forall(axiom.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${axiom.vertices}") // needed for autoId in expand
  assert((productions.values.flatMap(_.nonTerminals) ++ axiom.nonTerminals).forall { nonTerminal =>
    val rhs = productions.get(nonTerminal.label)
    rhs.isDefined && (nonTerminal.connectors.size == rhs.get.connectors.size)
  }, s"All existing nonterminals need to have an equivalent on the lhs: ${
    (productions.values.flatMap(_.nonTerminals) ++ axiom.nonTerminals).find { nonTerminal =>
      val rhs = productions.get(nonTerminal.label)
      rhs.isEmpty || (nonTerminal.connectors.size != rhs.get.connectors.size)
    }.get
  }\n$this")
  assert(!dependencyGraph.hasCycle, "this grammer contains cycles, which it shouldn't, so shit see this instead.")
  assert(axiom.connectors.isEmpty, "Axiom must not have connectors")
  assert(axiom.nonTerminals.nonEmpty, s"Axiom must have at least one non-terminal\n$this")
  // TODO: assert(productions.values.forall(_.nonTerminals.forall(_.connectors.nonEmpty)), "no empty nonterminals allowed in productions")

  def addProduction[V1 >: V, E1 >: E](production: (Label, Graph[V1, E1])): Grammar[V1, E1] = {
    assert(!(productions.keys.toSet contains production._1), s"productions already contain a rule with label ${production._1}\nproductions:\n${productions.mkString("\n")}")

    copy(productions = productions + production)
  }

  def removeProduction(nonTerminal: Label) = {
    // Recursive productions are not allowed, so check for all productions.
    assert(!productions.values.exists(_.nonTerminals.map(_.label) contains nonTerminal), "to-be-removed production is used in another production")
    assert(!(axiom.nonTerminals.map(_.label) contains nonTerminal), "to-be-removed production is used in axiom")
    copy(productions = productions - nonTerminal)
  }

  def updateProduction[V1 >: V, E1 >: E](production: (Label, Graph[V1, E1])): Grammar[V1, E1] = {
    val (label, graph) = production
    assert(productions contains label)
    copy(productions = productions.updated(label, graph))
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

  def uniqueVertices = {
    val autoId = AutoId(axiom.vertices.size)
    Grammar(axiom, productions.map{
      case (label, graph) =>
        val mapping = (graph.vertices.map(_.label) zip autoId).toMap
        label -> (graph map mapping)
    })
  }

  def expand: Graph[V, E] = expand(AutoId(axiom.vertices.size))
  @tailrec final def expand(autoId: AutoId): Graph[V, E] = {
    val nonTerminal = axiom.nonTerminals.head
    val replacement = productions(nonTerminal.label)
    val next = axiom.replaceOne(nonTerminal, replacement, autoId)
    if (next.nonTerminals.isEmpty) next
    else copy(axiom = next).expand(autoId)
  }

  def numElements = productions.values.map(g => g.numElements).sum
  def compressionRatio = numElements.toDouble / expand.numElements

  override def toString = s"Grammar(\n  Axiom: $axiom\n${
    productions.toList.sortBy(_._1).map{
      case (l, g) => s"  ${NonTerminal(l, g.connectors)} -> ${g.copy(connectors = Nil)}"
    }.mkString("\n")
  }\n)"
}
