package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.hypergraph._

import aiolia.helpers.Random

object Mutation {

  def mutate[V,E](grammar:Grammar[V,E]):Grammar[V,E] = {
    // pass count of mutations?
    // - choose production rule
    //    ? remove vertex
    //    ? remove edge
    //    ? remove hyperedge
    //    ? add vertex
    //    ? add edge
    //    ? add hyperedge
    //      ? reuse
    //        - choose existing hyperedge from left side of grammar, which does not produce a cycle
    //
    //      ? new
    //        - choose subgraph, replace by new hyperdge
    //        - create new production rule
    // prune
    ???
  }

  def removeRandomVertex[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    val (label, replacement) = random.select(grammar.productions)
    val vertexCandidates = replacement.hyperGraph.vertices -- replacement.connectors
    if( vertexCandidates.isEmpty ) return None

    val vertex = random.select(vertexCandidates)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement - vertex)))
  }

  def removeRandomEdge[V,E](grammar:Grammar[V,E], random:Random):Option[Grammar[V,E]] = {
    val (label, replacement) = random.select(grammar.productions)
    if( replacement.hyperGraph.edges.isEmpty ) return None

    val edge = random.select(replacement.hyperGraph.edges)
    Some(grammar.copy(productions = grammar.productions.updated(label, replacement - edge)))
  }
}
