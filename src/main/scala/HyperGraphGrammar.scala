package aiolia.hypergraphgrammar

import aiolia.helpers._
import aiolia.graph._
import aiolia.graph.types._
import aiolia.hypergraph._

case class MultiPointedHyperGraph[+E, +V](connectors: List[Vertex] = Nil, hyperGraph: HyperGraph[E, V] = HyperGraph()) {
  assert(connectors.forall(vertices contains _), "All tentacles must be used in HyperGraph")

  def vertices = hyperGraph.vertices
  def edges = hyperGraph.edges
  def hyperEdges = hyperGraph.hyperEdges
  def vertexData = hyperGraph.vertexData
  def edgeData = hyperGraph.edgeData

  def -(v:Vertex) = {
    assert(!(connectors contains v), s"Cannot remove connector vertex $v")

    copy( hyperGraph = hyperGraph - v )
  }

  def -(e:Edge) = copy( hyperGraph = hyperGraph - e )
  def -(h:HyperEdge) = copy( hyperGraph = hyperGraph - h )

  def +(v:Vertex) = copy(hyperGraph = hyperGraph + v)
  def +(e:Edge) = copy(hyperGraph = hyperGraph + e)
  def +(h:HyperEdge) = copy(hyperGraph = hyperGraph + h)
}

object Grammar {
  def replace[E,V](g:HyperGraph[E,V], h:HyperEdge, replacement:MultiPointedHyperGraph[E,V], autoId:AutoId):HyperGraph[E,V] = {
    // newly created vertices that will be merged into the graph at fringe vertices
    val newVertices = (replacement.vertices -- replacement.connectors).map(_.label -> Vertex(autoId.nextId)).toMap
    // existing fringe/connectivity vertices for merge process
    val existVertices = replacement.connectors.map(_.label).zip(h.connectors).toMap
    val vertexMap: Map[Label, Vertex] = newVertices ++ existVertices

    val vertices = replacement.vertices.map(v => vertexMap(v.label))

    val edges = replacement.edges.map { case Edge(Vertex(in), Vertex(out)) => Edge(vertexMap(in), vertexMap(out)) }

    val hyperEdges = replacement.hyperEdges.map { case HyperEdge(label, connectors) => HyperEdge(label, connectors.map(v => vertexMap(v.label))) }

    val vertexData = replacement.vertexData.map { case (Vertex(label), v) => vertexMap(label) -> v }.toMap
    val edgeData = replacement.edgeData.map { case (Edge(Vertex(in), Vertex(out)), v) => Edge(vertexMap(in), vertexMap(out)) -> v }.toMap

    HyperGraph(
      g.vertices ++ vertices,
      (g.hyperEdges ++ hyperEdges) diff List(h),
      g.edges ++ edges,
      g.vertexData ++ vertexData,
      g.edgeData ++ edgeData
    )
  }
}

case class Grammar[+E, +V](axiom: HyperGraph[E, V], productions: Map[Label, MultiPointedHyperGraph[E, V]] = Map.empty[Label, MultiPointedHyperGraph[E, V]]) {
  assert((0 until axiom.vertices.size).forall(axiom.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${axiom.vertices}") // needed for autoId in expand
  assert(productions.values.flatMap(_.hyperEdges).forall { hyperEdge =>
    val rhs = productions.get(hyperEdge.label)
    rhs.isDefined && (hyperEdge.connectors.size == rhs.get.connectors.size)
  }, "All hyperedges on the rhs need to have an equivalent on the lhs")
  assert(!dependencyGraph.hasCycle, "this grammer contains cycles, which it shouldn't, so shit see this instead.")

  def addProduction[E1 >: E, V1 >: V](production: (Label, MultiPointedHyperGraph[E1,V1])):Grammar[E1, V1] = {
    assert(!(productions.keys.toSet contains production._1), s"productions already contain a rule with label ${production._1}\nproductions:\n${productions.mkString("\n")}")

    copy(productions = productions + production)
  }

  def removeProduction(hyperEdge: Label) = {
    //TODO: assert: label should not be used in rhs of all productions
    copy(productions = productions - hyperEdge)
  }

  def dependencyGraph = {
    val vertices = productions.keys.map(Vertex(_)).toSet
    val edges = productions.flatMap { case (lhs, rhs) =>
      rhs.hyperEdges.map(h => Edge(Vertex(lhs), Vertex(h.label)))
    }.toSet

    Graph(vertices, edges)
  }

  def cleanup:Grammar[E,V] = {
    val starts:List[Label] = axiom.hyperEdges.map(_.label).distinct
    val keep:Set[Vertex] = starts.flatMap( start => dependencyGraph.depthFirstSearch(Vertex(start)) ).toSet
    copy(productions = productions.filterKeys(keep contains Vertex(_)))
  }

  def expand = {
    // TODO: make stateless - with a fold?
    var current = axiom
    val autoId = AutoId(axiom.vertices.size) // assuming that vertices have labels 0..n

    while (current.hyperEdges.nonEmpty) {
      val hyperEdge = current.hyperEdges.head
      val replacement = productions(hyperEdge.label)
      current = Grammar.replace(current, hyperEdge, replacement, autoId)
    }

    current.toGraph
  }
}

