package aioli

case class HyperGraph(hyperEdges: Set[HyperEdge] = Set.empty, edges: Set[Edge] = Set.empty, incoming:List[Vertex] = Nil, outgoing:List[Vertex] = Nil)
case class HyperEdge(label: Int, incoming:List[Vertex] = Nil, outgoing:List[Vertex] = Nil)
case class Edge(label: Int, incoming:Vertex, outgoing:Vertex)

case class Grammar(axiom: HyperGraph, productions: Map[HyperEdge,() => HyperGraph])
case class Vertex(label: Int)

trait AutoId {
  var id = 0
  def nextId = {id += 1; id}
}

object Vertex extends AutoId {
  def apply() = {
    Vertex(nextId)
  }
}

object HyperEdge extends AutoId{
  def apply(incoming: List[Vertex] = Nil, outgoing: List[Vertex] = Nil) = {
    HyperEdge(nextId, incoming, outgoing)
  }
}

object Edge extends AutoId{
  def apply(incoming: Vertex, outgoing: Vertex) = {
    Edge(nextId, incoming, outgoing)
  }
}

object Production {
  def apply(hyperEdge: HyperEdge, hyperGraph: () => HyperGraph) = {
    assert((hyperEdge.incoming ++ hyperEdge.outgoing).forall(hyperGraph.vertices contains _))
    hyperEdge -> () => HyperGraph(
      hyperGraph, hyperEdge.incoming, hyperEdge.outgoing)

  }
}


object Main extends App {
  val (v1, v2, v3) = (Vertex(), Vertex(), Vertex())
  val h = HyperEdge(List(v1), List(v2))
  val axiom = HyperGraph(
    Set(v1, v2),
    Set()
  )

  val g = Grammar(axiom, Map(
    Production(h, () => {
      HyperGraph(Set(h.incoming.head, h.outgoing.head), edges = Set(h.incoming.head, h.outgoing.head))
    })
    ))
}
