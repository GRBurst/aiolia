package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.hypergraph._

import aiolia.helpers.{Random, AutoId}

object Mutation {

  def mutate[V,E](grammar:Grammar[V,E]):Grammar[V,E] = {
    // pass count of mutations?
    // - choose production rule
    //    ? add vertex
    //    ? add edge
    //    ? reuse hyperedge
    //    ? extract hyperedge
    //    ? remove vertex
    //    ? remove edge
    //    ? inline hyperedge
    //
    //      ? reuse
    //        - choose existing hyperedge from left side of grammar, which does not produce a cycle
    //
    //      ? new
    //        - choose subgraph, replace by new hyperdge
    //        - create new production rule
    // prune
    ???
  }

  def extractHyperEdge[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    if(grammar.productions.size < 2) return None

    val (srcLabel, source) = random.select(grammar.productions)

    val subVertices = random.select(source.vertices, n = random.r.nextInt(source.vertices.size))
    val connectors = source.hyperGraph.neighbours(subVertices).toList // order does not matter, it just needs to be the same in newHyperEdge and newRule rhs
    val subGraph = source.hyperGraph.inducedSubGraph(subVertices)
    val newLabel = grammar.productions.keys.max + 1
    val newHyperEdge = HyperEdge(newLabel, connectors)
    //TODO: translate vertices to local ones for newRule? (in connectors and subGraph), maybe implement HyperGraph.map, Graph.map (in HyperGraph.map, reuse Graph.map. In general, reuse many algorithms of Graph)
    val newRule = (newLabel, MultiPointedHyperGraph(connectors, subGraph))

    Some(grammar.copy(productions = grammar.productions.updated(srcLabel, source -- subGraph + newHyperEdge) + newRule))
  }

  def reuseHyperEdge[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    if(grammar.productions.size < 2) return None

    val (srcLabel, source) = random.select(grammar.productions)
    val (targetLabel, target) = random.select(grammar.productions - srcLabel)

    if(target.vertices.size < source.connectors.size) return None

    val connectors = random.select(target.vertices, n = source.connectors.size).toList // TODO: shuffle connectors?
    val hyperEdge = HyperEdge(srcLabel, connectors)

    Some(grammar.copy(productions = grammar.productions.updated(targetLabel, target + hyperEdge)))
  }

  def addRandomEdge[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    if(grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)

    if(replacement.vertices.size < 2) return None

    val vertexIn = random.select(replacement.vertices)
    val vertexOut = random.select(replacement.vertices - vertexIn)
    val edge = Edge(vertexIn, vertexOut)

    Some(grammar.copy(productions = grammar.productions.updated(label, replacement + edge)))
  }

  def addRandomVertex[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    if(grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    val vertexLabel = replacement.vertices.maxBy(_.label).label + 1
    val vertex = Vertex(vertexLabel)

    Some(grammar.copy(productions = grammar.productions.updated(label, replacement + vertex)))
  }

  def removeRandomVertex[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    if(grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    val vertexCandidates = replacement.hyperGraph.vertices -- replacement.connectors
    if( vertexCandidates.isEmpty ) return None

    val vertex = random.select(vertexCandidates)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement - vertex)))
  }

  def removeRandomEdge[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    if(grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    if( replacement.hyperGraph.edges.isEmpty ) return None

    val edge = random.select(replacement.hyperGraph.edges)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement - edge)))
  }
  //TODO? def removeRandomHyperEdge[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]]

  def inlineRandomHyperEdge[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    if(grammar.productions.isEmpty) return None

    val (label, replacement) = random.select(grammar.productions)
    if( replacement.hyperEdges.isEmpty ) return None

    val hyperEdge = random.select(replacement.hyperEdges)

    // TODO: avoid maxBy in default AutoId?
    val autoId = new AutoId(start = replacement.vertices.maxBy(_.label).label + 1)
    val inlined = Grammar.replace(replacement.hyperGraph, hyperEdge, grammar.productions(hyperEdge.label), autoId)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement.copy(hyperGraph = inlined))))
  }
}
