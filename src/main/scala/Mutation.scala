package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import util.Try

object Mutation {
  // TODO: Mutate is still not perfectly deterministic!
  // This can happen when iterating over HashSets, MashMaps ...
  type MutOp[V, E] = (Grammar[V, E], Random) => Option[Grammar[V, E]]

  def directedGraphOperators[V, E]: List[MutOp[V, E]] = List(
    addVertex,
    addEdge,
    removeVertex,
    removeEdge,
    inlineNonTerminal,
    extractNonTerminal,
    reuseNonTerminal
  )

  def directedConnectedGraphOperators[V, E]: List[MutOp[V, E]] = List(
    addConnectedVertex,
    addEdge,
    //TODO: removeConnectedVertex,
    //TODO: removeConnectedEdge,
    inlineNonTerminal,
    extractNonTerminal,
    reuseNonTerminal
  )

  def directedAcyclicConnectedGraphOperators[V, E]: List[MutOp[V, E]] = List(
    //TODO add/remove acyclic vertex/edge
    inlineNonTerminal,
    extractNonTerminal,
    reuseNonTerminal
  )

  def mutateDirected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    mutate(grammar, directedGraphOperators, random, n)
  }

  def mutateDirectedConnected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    assert(grammar.expand.isConnected)
    val newGrammar = mutate(grammar, directedConnectedGraphOperators, random, n)
    assert(newGrammar.expand.isConnected)
  }

  def mutateAcyclicConnected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    assert(grammar.expand.isConnected)
    assert(!grammar.expand.hasCycle)
    val newGrammar = mutate(grammar, directedConnectedGraphOperators, random, n)
    assert(newGrammar.expand.isConnected)
    assert(!newGrammar.expand.hasCycle)
  }

  private def mutate[V, E](grammar: Grammar[V, E], operators: List[MutOp[V, E]], random: Random, n: Int): Grammar[V, E] = {
    //TODO: assert constraints after every mutation
    var current = grammar
    var mutations = 0
    while (mutations < n) {
      random.select(operators)(current, random) foreach { newGrammar =>
        current = newGrammar
        mutations += 1
      }
    }
    current.cleanup
  }

  def addVertex[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    // println("addVertex")

    val (label, replacement) = random.select(grammar.productions)
    val vertexLabel = Try(replacement.vertices.maxBy(_.label).label + 1).getOrElse(0)
    val vertex = Vertex(vertexLabel)

    Some(grammar.updateProduction(label -> (replacement + vertex)))
  }

  def addConnectedVertex[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    // println("addConnectedVertex")

    val (label, replacement) = random.select(grammar.productions)
    val vertexLabel = Try(replacement.vertices.maxBy(_.label).label + 1).getOrElse(0)
    val newVertex = Vertex(vertexLabel)
    val existingVertex = random.select(replacement.vertices)
    val newEdge = if (random.r.nextBoolean) Edge(newVertex, existingVertex) else Edge(existingVertex, newVertex)

    Some(grammar.updateProduction(label -> (replacement + newVertex + newEdge)))
  }

  def addEdge[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    // println("addEdge")

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

  def removeVertex[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    // println("removeVertex")

    val candidates = grammar.productions.filter(_._2.nonConnectors.nonEmpty)
    if (candidates.isEmpty) return None

    val (label, replacement) = random.select(candidates)
    val vertexCandidates = replacement.nonConnectors

    val vertex = random.select(vertexCandidates)
    Some(grammar.updateProduction(label -> (replacement - vertex)))
  }

  def removeEdge[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    // println("removeEdge")

    val candidates = grammar.productions.filter(_._2.edges.nonEmpty)
    if (candidates.isEmpty) return None

    val (label, replacement) = random.select(candidates)
    val edgeCandidates = replacement.edges

    val edge = random.select(edgeCandidates)
    Some(grammar.updateProduction(label -> (replacement - edge)))
  }
  //TODO? def removeNonTerminal[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]]

  def inlineNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    // println("inlineNonTerminal")

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

  def extractNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    // println("extractNonTerminal")
    // if (grammar.productions.size < 2) return None

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

  // Apply random production rule to another production rule's graph
  def reuseNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    // println("reuseNonTerminal")
    if (grammar.productions.size < 2) return None

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
