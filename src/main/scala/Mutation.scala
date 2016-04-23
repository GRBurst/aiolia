package aiolia

import aiolia.graph._
import aiolia.helpers.{Random, AutoId}

object Mutation {

  def mutate[V, E](grammar: Grammar[V, E]): Grammar[V, E] = {
    // pass count of mutations?
    // - choose production rule
    //    ? add vertex
    //    ? add edge
    //    ? reuse nonterminal
    //    ? extract nonterminal
    //    ? remove vertex
    //    ? remove edge
    //    ? inline nonterminal
    //
    //      ? reuse
    //        - choose existing nonterminal from left side of grammar, which does not produce a cycle
    //
    //      ? new
    //        - choose subgraph, replace by new nonterminal
    //        - create new production rule
    // prune
    ???
  }

  def extractNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    if (grammar.productions.size < 2) return None

    val (srcLabel, source) = random.select(grammar.productions)

    val subVertices = random.select(source.vertices, n = random.r.nextInt(source.vertices.size))
    val connectors = source.neighbours(subVertices).toList // order does not matter, it just needs to be the same in newNonTerminal and newRule rhs
    val subGraph = source.inducedSubGraph(subVertices)
    val newLabel = grammar.productions.keys.max + 1
    val newNonTerminal = NonTerminal(newLabel, connectors)
    //TODO: translate vertices to local ones for newRule? (in connectors and subGraph), maybe implement Graph.map, Graph.map (in Graph.map, reuse Graph.map. In general, reuse many algorithms of Graph)
    val newRule = (newLabel, subGraph.copy(connectors = connectors))

    Some(grammar.copy(productions = grammar.productions.updated(srcLabel, source -- subGraph + newNonTerminal) + newRule))
  }

  def reuseNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    if (grammar.productions.size < 2) return None

    val (srcLabel, source) = random.select(grammar.productions)
    val (targetLabel, target) = random.select(grammar.productions - srcLabel)

    if (target.vertices.size < source.connectors.size) return None

    val connectors = random.select(target.vertices, n = source.connectors.size).toList // TODO: shuffle connectors?
    val nonTerminal = NonTerminal(srcLabel, connectors)

    Some(grammar.copy(productions = grammar.productions.updated(targetLabel, target + nonTerminal)))
  }

  def addRandomEdge[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    if (grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)

    if (replacement.vertices.size < 2) return None

    val vertexIn = random.select(replacement.vertices)
    val vertexOut = random.select(replacement.vertices - vertexIn)
    val edge = Edge(vertexIn, vertexOut)

    Some(grammar.copy(productions = grammar.productions.updated(label, replacement + edge)))
  }

  def addRandomVertex[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    if (grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    val vertexLabel = replacement.vertices.maxBy(_.label).label + 1
    val vertex = Vertex(vertexLabel)

    Some(grammar.copy(productions = grammar.productions.updated(label, replacement + vertex)))
  }

  def removeRandomVertex[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    if (grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    val vertexCandidates = replacement.vertices -- replacement.connectors
    if (vertexCandidates.isEmpty) return None

    val vertex = random.select(vertexCandidates)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement - vertex)))
  }

  def removeRandomEdge[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    if (grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    if (replacement.edges.isEmpty) return None

    val edge = random.select(replacement.edges)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement - edge)))
  }
  //TODO? def removeRandomNonTerminal[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]]

  def inlineRandomNonTerminal[V, E](grammar: Grammar[V, E], random: Random): Option[Grammar[V, E]] = {
    if (grammar.productions.isEmpty) return None

    val (label, target) = random.select(grammar.productions)
    if (target.nonTerminals.isEmpty) return None

    val nonTerminal = random.select(target.nonTerminals)

    // TODO: avoid maxBy in default AutoId?
    val autoId = new AutoId(start = target.vertices.maxBy(_.label).label + 1)
    val inlined = Grammar.replace(target, nonTerminal, grammar.productions(nonTerminal.label), autoId)
    Some(grammar.copy(productions = grammar.productions.updated(label, inlined)))
  }
}
