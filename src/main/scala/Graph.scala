package aiolia.graph

package object types {
  type Label = Int
}

import types._
import collection.mutable

//TODO: make Vertex a value class?
case class Vertex(label: Label) {
  override def toString = s"$label"
}

case class Edge(in: Vertex, out: Vertex) {
  def contains(v: Vertex) = in == v || out == v
  override def toString = s"${in.label} -> ${out.label}"
}

case class NonTerminal(label: Label, connectors: List[Vertex] = Nil) {
  //TODO: NonTerminal: should connectors be distinct?
  def contains(v: Vertex) = connectors contains v
  override def toString = s"[$label:${connectors.mkString("-")}]"
}

object Graph {
  val empty = Graph()
}

case class Graph[+V, +E](
    vertices:     Set[Vertex]       = Set.empty,
    edges:        Set[Edge]         = Set.empty,
    vertexData:   Map[Vertex, V]    = Map.empty[Vertex, V],
    edgeData:     Map[Edge, E]      = Map.empty[Edge, E],
    nonTerminals: List[NonTerminal] = Nil,
    connectors:   List[Vertex]      = Nil
//TODO: order on List[NonTerminal] should not matter, especially on comparison. We probably need a "Bag" datastructure: https://github.com/nicolasstucki/multisets
) {

  //TODO: Graph: should connectors be distinct?
  //TODO: allow selfloops?
  assert(edges.flatMap(e => List(e.in, e.out)) subsetOf vertices, "All vertices used in edges have to be defined")
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only contain vertices in Graph")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only contain edges in Graph")
  assert(nonTerminals.flatMap(_.connectors).toSet subsetOf vertices, "All vertices used in nonTerminals have to be defined")
  assert(connectors.toSet subsetOf vertices, "All connectors must be used in Graph")

  def subGraphOf[V1 >: V, E1 >: E](that: Graph[V1, E1]) = {
    (this.vertices subsetOf that.vertices) &&
      (this.edges subsetOf that.edges) &&
      (this.nonTerminals.toSet subsetOf that.nonTerminals.toSet)
  }

  // def successors(v: Vertex) = edges.collect { case Edge(`v`, out) => out }
  // def predecessors(v: Vertex) = edges.collect { case Edge(in, `v`) => in }
  // def outgoingEdges(v: Vertex) = edges.filter(_.in == v)
  // def incomingEdges(v: Vertex) = edges.filter(_.out == v)
  private def MapVVempty = Map.empty[Vertex, Set[Vertex]].withDefaultValue(Set.empty)
  private def MapVEempty = Map.empty[Vertex, Set[Edge]].withDefaultValue(Set.empty)
  lazy val successors: Map[Vertex, Set[Vertex]] = edges.foldLeft(MapVVempty){ case (suc, Edge(in, out)) => suc + (in → (suc(in) + out)) }
  lazy val predecessors: Map[Vertex, Set[Vertex]] = edges.foldLeft(MapVVempty){ case (pre, Edge(in, out)) => pre + (out → (pre(out) + in)) }
  lazy val incomingEdges: Map[Vertex, Set[Edge]] = edges.foldLeft(MapVEempty){ case (incoming, edge @ Edge(_, out)) => incoming + (out → (incoming(out) + edge)) }
  lazy val outgoingEdges: Map[Vertex, Set[Edge]] = edges.foldLeft(MapVEempty){ case (outgoing, edge @ Edge(in, _)) => outgoing + (in → (outgoing(in) + edge)) }

  def +(v: Vertex) = {
    assert(!(vertices contains v), s"Vertex $v already exists in ${vertices}")
    copy(vertices = vertices + v)
  }

  def +(e: Edge) = {
    assert(!(edges contains e), s"Edge $e already exists in ${edges}")
    copy(edges = edges + e)
  }

  def +(h: NonTerminal) = copy(nonTerminals = h :: nonTerminals)

  def -(v: Vertex) = {
    assert(vertices contains v, s"Vertex $v does not exist in ${vertices}")
    assert(!(connectors contains v), s"Cannot remove connector vertex $v")

    val (removedEdges, retainedEdges) = edges.partition(_ contains v)
    Graph(
      vertices - v,
      retainedEdges,
      vertexData - v,
      edgeData -- removedEdges,
      nonTerminals.filterNot(_.connectors contains v),
      connectors
    )
  }

  def -(e: Edge) = {
    assert(edges contains e, s"Edge $e does not exist in ${edges}")
    copy(edges = edges - e, edgeData = edgeData - e)
  }

  def -(h: NonTerminal) = {
    assert(nonTerminals contains h, s"NonTerminal $h does not exist in ${nonTerminals}")
    val i = nonTerminals indexOf h
    copy(nonTerminals = nonTerminals.take(i) ++ nonTerminals.drop(i + 1))
  }

  def neighbours(v: Vertex): Set[Vertex] = edges.collect {
    case Edge(`v`, out) => out
    case Edge(in, `v`)  => in
  }
  // open neighbourhood
  def neighbours(vs: Iterable[Vertex]): Set[Vertex] = vs.flatMap(neighbours).toSet -- vs

  def incidentEdges(v: Vertex): Set[Edge] = edges.filter(_ contains v)
  def incidentEdges(vs: Iterable[Vertex]): Set[Edge] = vs.flatMap(incidentEdges).toSet

  def inducedEdges(vs: Iterable[Vertex]): Set[Edge] = {
    val vsSet = vs.toSet
    edges.filter(e => (vsSet contains e.in) && (vsSet contains e.out))
  }

  def incidentNonTerminals(v: Vertex): List[NonTerminal] = nonTerminals.filter(_ contains v)
  def incidentNonTerminals(vs: Iterable[Vertex]): List[NonTerminal] = {
    val vsSet = vs.toSet
    nonTerminals.filter(_.connectors.exists(vsSet contains _))
  }
  def inducedNonTerminals(vs: Iterable[Vertex]): List[NonTerminal] = {
    val vsSet = vs.toSet
    nonTerminals.filter(_.connectors.toSet subsetOf vsSet)
  }

  def inducedSubGraph(vs: Iterable[Vertex]): Graph[V, E] = {
    val vsSet = vs.toSet
    Graph(vsSet, inducedEdges(vsSet), nonTerminals = inducedNonTerminals(vsSet))
  }

  def --[E1, V1](subGraph: Graph[E1, V1]) = {
    assert(subGraph subGraphOf this)
    assert(subGraph.connectors.isEmpty)

    val removedVertices = subGraph.vertices
    val removedEdges = subGraph.edges ++ incidentEdges(subGraph.vertices)
    val removedNonTerminals = subGraph.nonTerminals ++ incidentNonTerminals(subGraph.vertices)

    Graph(
      vertices -- removedVertices,
      edges -- removedEdges,
      vertexData -- removedVertices,
      edgeData -- removedEdges,
      nonTerminals diff removedNonTerminals
    )
  }

  def depthFirstSearch(start: Vertex, revSort: Set[Vertex] => Iterable[Vertex] = set => set) = new Iterator[Vertex] {
    assert(vertices contains start)
    val stack = mutable.Stack(start)
    val seen = mutable.Set[Vertex]()

    override def hasNext: Boolean = stack.nonEmpty
    override def next: Vertex = {
      val current = stack.pop
      seen += current
      stack pushAll revSort(successors(current) filterNot ((seen ++ stack) contains))
      current
    }
  }

  def hasCycle: Boolean = {
    val next = mutable.HashSet.empty ++ vertices

    while (next.nonEmpty) {
      if (cycleAt(next.head)) return true
    }

    def cycleAt(v: Vertex, visited: Set[Vertex] = Set.empty): Boolean = {
      if (visited contains v) return true // found cycle
      if (!(next contains v)) return false // we already checked from here, there is definitely no cycle

      next -= v
      successors(v).exists(cycleAt(_, visited + v))
    }

    return false
  }

  override def toString = s"Graph(V(${vertices.toList.sortBy(_.label).mkString(" ")}), " +
    s"E(${edges.toList.sortBy(_.out.label).sortBy(_.in.label).mkString(", ")})" +
    (if (vertexData.nonEmpty) s", {${vertexData.toList.sortBy(_._1.label).map{ case (v, d) => s"$v: $d" }.mkString(", ")}}" else "") +
    (if (edgeData.nonEmpty) s", {${edgeData.toList.sortBy(_._1.out.label).sortBy(_._1.in.label).map{ case (Edge(in, out), d) => s"$in->$out: $d" }.mkString(", ")}}" else "") +
    (if (nonTerminals.nonEmpty) s", NTS(${nonTerminals.mkString(", ")})" else "") +
    (if (connectors.nonEmpty) s", C(${connectors.mkString("-")})" else "") +
    ")"

}
