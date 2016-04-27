package aiolia

import aiolia.graph._
import aiolia.helpers.{Random, AutoId}

object Mutation {
  type MutOp[V, E] = (Grammar[V, E], Random) => Option[Grammar[V, E]]

  def mutate[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1): Grammar[V, E] = {
    val operations: List[MutOp[V, E]] = List(
      addVertex,
      addEdge,
      // removeVertex,
      // removeEdge,
      inlineNonTerminal,
      extractNonTerminal,
      reuseNonTerminal
    )
    var current = grammar
    var mutations = 0
    while (mutations < n) {
      random.select(operations)(current, random) foreach { newGrammar =>
        println(newGrammar)
        current = newGrammar
        mutations += 1
      }
    }

    current.cleanup
  }

  def addVertex[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    println("addVertex")
    if (grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    val vertexLabel = if (replacement.vertices.isEmpty) 0 else replacement.vertices.maxBy(_.label).label + 1
    val vertex = Vertex(vertexLabel)

    Some(grammar.copy(productions = grammar.productions.updated(label, replacement + vertex)))
  }

  def addEdge[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    println("addEdge")
    if (grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)

    if (replacement.vertices.size < 2) return None
    val connectableVertices = replacement.vertices.filter(v => replacement.successors(v).size < replacement.vertices.size - 1)
    if (connectableVertices.isEmpty) return None // fully connected graph

    val vertexIn = random.select(connectableVertices)
    val vertexOut = random.select(replacement.vertices - vertexIn -- replacement.successors(vertexIn))
    val edge = Edge(vertexIn, vertexOut)

    Some(grammar.copy(productions = grammar.productions.updated(label, replacement + edge)))
  }

  def removeVertex[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    println("removeVertex")
    if (grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    val vertexCandidates = replacement.vertices -- replacement.connectors
    if (vertexCandidates.isEmpty) return None

    val vertex = random.select(vertexCandidates)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement - vertex)))
  }

  def removeEdge[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    println("removeEdge")
    if (grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    if (replacement.edges.isEmpty) return None

    val edge = random.select(replacement.edges)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement - edge)))
  }
  //TODO? def removeNonTerminal[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]]

  def inlineNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    println("inlineNonTerminal")
    if (grammar.productions.isEmpty) return None

    val (label, target) = random.select(grammar.productions)
    if (target.nonTerminals.isEmpty) return None

    val nonTerminal = random.select(target.nonTerminals)

    // TODO: avoid maxBy in default AutoId?
    val autoId = new AutoId(start = target.vertices.maxBy(_.label).label + 1)
    val inlined = target.replaceOne(nonTerminal, grammar.productions(nonTerminal.label), autoId)
    Some(grammar.copy(productions = grammar.productions.updated(label, inlined)))
  }

  def extractNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    println("extractNonTerminal")
    if (grammar.productions.size < 2) return None

    val (srcLabel, source) = random.select(grammar.productions)
    if (source.nonConnectors.isEmpty) return None

    val subVertices = random.select(source.nonConnectors, n = random.r.nextInt(source.nonConnectors.size - 1) + 1)

    val connectors = source.neighbours(subVertices).toList // order does not matter, it just needs to be the same in newNonTerminal and newRule rhs
    val subGraph = source.inducedSubGraph(subVertices)
    val newLabel = grammar.productions.keys.max + 1
    val newNonTerminal = NonTerminal(newLabel, connectors)
    //TODO: translate vertices to local ones for newRule? (in connectors and subGraph), maybe implement Graph.map, Graph.map (in Graph.map, reuse Graph.map. In general, reuse many algorithms of Graph)
    val newRule = (newLabel, subGraph.copy(connectors = connectors))

    Some(grammar.copy(productions = grammar.productions.updated(srcLabel, source -- subGraph + newNonTerminal) + newRule))
  }

  def reuseNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    println("reuseNonTerminal")
    if (grammar.productions.size < 2) return None

    val (srcLabel, source) = random.select(grammar.productions)
    val validProductions = (grammar.productions - srcLabel).filter { case (label, g) => g.vertices.size < source.connectors.size }
    if (validProductions.isEmpty) return None
    val (targetLabel, target) = random.select(validProductions)

    val connectors = random.select(target.vertices, n = source.connectors.size).toList
    val nonTerminal = NonTerminal(srcLabel, connectors)

    Some(grammar.copy(productions = grammar.productions.updated(targetLabel, target + nonTerminal)))
  }
}
