package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.hypergraph._

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


}
