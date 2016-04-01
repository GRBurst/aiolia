package aiolia.hypergraphgrammar

import aiolia.graph._
import aiolia.hypergraph._

object Mutation {

  def mutate[V,E](grammar:Grammar[V,E]):Grammar[V,E] = {
    ???
  }

  def removeVertex[V,E](mphgi:MultiPointedHyperGraph[V,E], v:Vertex):MultiPointedHyperGraph[V,E] = {
    assert(mphgi.hyperGraph.vertices contains v, s"Vertex $v does not exist in ${mphgi.hyperGraph.vertices}")
    assert(!(mphgi.in contains v), s"Cannot remove input vertex $v")
    assert(!(mphgi.out contains v), s"Cannot remove output vertex $v")
    import mphgi.hyperGraph._

    mphgi.copy(
      hyperGraph = HyperGraph(
          vertices - v,
          hyperEdges.filterNot(h => (h.in contains v) || (h.out contains v)),
          edges.filterNot(e => e.in == v || e.out == v)
        )
      )
  }

  def removeEdge[V,E](mphgi:MultiPointedHyperGraph[V,E], e:Edge):MultiPointedHyperGraph[V,E] = {
    assert(mphgi.hyperGraph.edges contains e, s"Edge $e does not exist in ${mphgi.hyperGraph.edges}")
    import mphgi.hyperGraph._

    mphgi.copy(
      hyperGraph = HyperGraph(
          vertices,
          hyperEdges,
          edges - e
        )
      )
  }

  def removeHyperEdge[V,E](mphgi:MultiPointedHyperGraph[V,E], h:HyperEdge):MultiPointedHyperGraph[V,E] = {
    assert(mphgi.hyperGraph.hyperEdges contains h, s"HyperEdge $h does not exist in ${mphgi.hyperGraph.hyperEdges}")
    import mphgi.hyperGraph._

    val i = hyperEdges indexOf h

    mphgi.copy(
      hyperGraph = HyperGraph(
          vertices,
          hyperEdges.take(i) ++ hyperEdges.drop(i+1),
          edges
        )
      )
  }

}
