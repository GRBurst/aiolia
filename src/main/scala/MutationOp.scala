package aiolia.mutations

import aiolia.Grammar
import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import util.Try

trait MutationOp {
  def apply[V,E](grammar: Grammar[V,E], random: Random): Option[Grammar[V,E]]
}

object AddVertex extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V,E]] = {
    val (label, replacement) = random.select(grammar.productions)
    val vertexLabel = Try(replacement.vertices.maxBy(_.label).label + 1).getOrElse(0)
    val vertex = Vertex(vertexLabel)

    Some(grammar.updateProduction(label -> (replacement + vertex)))
  }
}

object AddConnectedVertex extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V,E]] = {
    val (label, replacement) = random.select(grammar.productions)
    val vertexLabel = Try(replacement.vertices.maxBy(_.label).label + 1).getOrElse(0)
    val newVertex = Vertex(vertexLabel)
    val existingVertex = random.select(replacement.vertices)
    val newEdge = if (random.r.nextBoolean) Edge(newVertex, existingVertex) else Edge(existingVertex, newVertex)

    Some(grammar.updateProduction(label -> (replacement + newVertex + newEdge)))
  }
}

object AddEdge extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V,E]] = {
    val candidates = grammar.productions.filter(_._2.vertices.size >= 2)
    if (candidates.isEmpty) return None

    val (label, replacement) = random.select(candidates)
    // Only choose vertices that are not fully connected (to all other nodes)
    val startVertexCandidates = replacement.vertices.filter(v => replacement.outDegree(v) < replacement.vertices.size - 1)
    if (startVertexCandidates.isEmpty) return None // fully connected graph

    val vertexIn = random.select(startVertexCandidates)
    val vertexOut = random.select(replacement.vertices - vertexIn -- replacement.successors(vertexIn))
    val edge = Edge(vertexIn, vertexOut)

    Some(grammar.updateProduction(label -> (replacement + edge)))
  }
}

object RemoveVertex extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V,E]] = {
    val candidates = grammar.productions.filter(_._2.nonConnectors.nonEmpty)
    if (candidates.isEmpty) return None

    val (label, replacement) = random.select(candidates)
    val vertexCandidates = replacement.nonConnectors

    val vertex = random.select(vertexCandidates)
    Some(grammar.updateProduction(label -> (replacement - vertex)))
  }
}

object RemoveEdge extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V,E]] = {
    val candidates = grammar.productions.filter(_._2.edges.nonEmpty)
    if (candidates.isEmpty) return None

    val (label, replacement) = random.select(candidates)
    val edgeCandidates = replacement.edges

    val edge = random.select(edgeCandidates)
    Some(grammar.updateProduction(label -> (replacement - edge)))
  }
}

//TODO? def removeNonTerminal[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]]

object InlineNonTerminal extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V,E]] = {
    val candidates = grammar.productions.filter(_._2.nonTerminals.nonEmpty)
    if (candidates.isEmpty) return None

    val (label, graph) = random.select(candidates)
    val terminalCandidates = graph.nonTerminals
    val nonTerminal = random.select(terminalCandidates)

    // TODO: avoid maxBy in default AutoId?
    val autoId = new AutoId(start = Try(graph.vertices.maxBy(_.label).label + 1).getOrElse(0))
    val inlined = graph.replaceOne(nonTerminal, grammar.productions(nonTerminal.label), autoId)
    Some(grammar.updateProduction(label -> inlined))
  }
}

object ExtractNonTerminal extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V,E]] = {
    val candidates = grammar.productions.filter(_._2.vertices.nonEmpty)
    if (candidates.isEmpty) return None

    val (srcLabel, source) = random.select(candidates)
    val vertexCandidates = source.vertices

    val subVertices = random.select(vertexCandidates, n = random.nextInt(1, vertexCandidates.size + 1)) // min 1, max -1
    assert(subVertices.size > 0 && subVertices.size <= vertexCandidates.size)

    // val connectors = source.neighbours(subVertices).toList // order does not matter, it just needs to be the same in newNonTerminal and newRule rhs
    val connectorCandidates = subVertices
    val connectors = random.select(connectorCandidates, random.nextInt(1, connectorCandidates.size + 1)).toList
    assert(connectors.nonEmpty)
    val subGraph = source.inducedSubGraph(subVertices ++ connectors).copy(connectors = connectors)
    val newLabel = grammar.productions.keys.max + 1
    val newNonTerminal = NonTerminal(newLabel, connectors)
    //TODO: translate vertices to local ones for newRule? (in connectors and subGraph), maybe implement Graph.map, Graph.map (in Graph.map, reuse Graph.map. In general, reuse many algorithms of Graph)
    val newRule = (newLabel, subGraph)

    Some(grammar.addProduction(newRule).updateProduction(srcLabel -> (source -- (subGraph.nonConnectors -- source.connectors) + newNonTerminal)))
  }
}

// Apply random production rule to another production rule's graph
object ReuseNonTerminal extends MutationOp {
  override def apply[V,E](grammar: Grammar[V, E], random: Random): Option[Grammar[V,E]] = {
    val candidates = grammar.productions.toList.combinations(2).filter {
      //TODO: avoid cycles in grammar: check for path in dependencyGraph from target to source
      case List((_, source), (_, target)) => source.connectors.size <= target.vertices.size
    }.toList //TODO: optimize

    if (candidates.isEmpty) return None
    val List((srcLabel, source), (targetLabel, target)) = random.select(candidates)

    //TODO: target.vertices or target.nonConnectors ?
    val connectors = random.select(target.vertices, n = source.connectors.size).toList
    val nonTerminal = NonTerminal(srcLabel, connectors)

    //TODO: cycle detection!
    Try(grammar.updateProduction(targetLabel -> (target + nonTerminal))).toOption
  }
}
