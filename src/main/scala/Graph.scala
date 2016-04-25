package aiolia.graph

import aiolia.helpers.AutoId

package object types {
  type Label = Int
}

import types._
import collection.mutable

//TODO: make Vertex a value class? (problems with mockito)
// case class Vertex(label: Label) extends AnyVal {
case class Vertex(label: Label) {
  override def toString = s"$label"
}

case class Edge(in: Vertex, out: Vertex) {
  assert(in != out, "Self loops are not allowed")
  def contains(v: Vertex) = in == v || out == v
  override def toString = s"${in.label} -> ${out.label}"
  def toSet = Set(in, out)
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
  assert(edges.flatMap(e => List(e.in, e.out)) subsetOf vertices, "Edges can only connect existing vertices")
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only be attached to existing vertices")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only be attached to existing edges")
  assert(nonTerminals.flatMap(_.connectors).toSet subsetOf vertices, "NonTerminals can only connect existing vertices")
  //TODO: nonterminals with same label must have the same connectors (?)
  assert(connectors.toSet subsetOf vertices, "Only existing vertices can be used as connectors")
  assert((vertexData.keySet intersect connectors.toSet).isEmpty, "Connectors cannot store data")
  // assert((edgeData.keySet.flatMap(_.toSet) intersect connectors.toSet).isEmpty, "Edges incident to connectors cannot store data")
  // assert((nonTerminals.flatMap(_.connectors.toSet).toSet intersect connectors.toSet).isEmpty, "NonTerminals cannot connect connectors")

  lazy val nonConnectors = vertices -- connectors

  def subGraphOf[V1 >: V, E1 >: E](that: Graph[V1, E1]) = {
    (this.vertices subsetOf that.vertices) &&
      (this.edges subsetOf that.edges) &&
      (this.nonTerminals.toSet subsetOf that.nonTerminals.toSet)
  }

  def successors(v: Vertex) = { assert(vertices contains v); edges.collect { case Edge(`v`, out) => out } }
  def predecessors(v: Vertex) = { assert(vertices contains v); edges.collect { case Edge(in, `v`) => in } }
  def outgoingEdges(v: Vertex) = { assert(vertices contains v); edges.filter(_.in == v) }
  def incomingEdges(v: Vertex) = { assert(vertices contains v); edges.filter(_.out == v) }

  // lazy caching datastructure for successors, predecessors, incomingEdges, outgoingEdges
  // private def MapVVempty = Map.empty[Vertex, Set[Vertex]].withDefault((v: Vertex) => { assert(vertices contains v); Set.empty[Vertex] })
  // private def MapVEempty = Map.empty[Vertex, Set[Edge]].withDefault((v: Vertex) => { assert(vertices contains v); Set.empty[Edge] })

  // lazy val successors: Map[Vertex, Set[Vertex]] = edges.foldLeft(MapVVempty){ case (suc, Edge(in, out)) => suc + (in -> (suc(in) + out)) }
  // lazy val predecessors: Map[Vertex, Set[Vertex]] = edges.foldLeft(MapVVempty){ case (pre, Edge(in, out)) => pre + (out -> (pre(out) + in)) }
  // lazy val incomingEdges: Map[Vertex, Set[Edge]] = edges.foldLeft(MapVEempty){ case (incoming, edge @ Edge(_, out)) => incoming + (out -> (incoming(out) + edge)) }
  // lazy val outgoingEdges: Map[Vertex, Set[Edge]] = edges.foldLeft(MapVEempty){ case (outgoing, edge @ Edge(in, _)) => outgoing + (in -> (outgoing(in) + edge)) }

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

  def neighbours(v: Vertex): Set[Vertex] = {
    assert(vertices contains v)
    edges.collect {
      case Edge(`v`, out) => out
      case Edge(in, `v`)  => in
    }
  }
  // open neighbourhood
  def neighbours(vs: Iterable[Vertex]): Set[Vertex] = {
    assert(vs.toSet subsetOf vertices)
    vs.flatMap(neighbours).toSet -- vs
  }
  def incidentEdges(v: Vertex): Set[Edge] = {
    assert(vertices contains v)
    edges.filter(_ contains v)
  }
  def incidentEdges(vs: Iterable[Vertex]): Set[Edge] = {
    assert(vs.toSet subsetOf vertices)
    vs.flatMap(incidentEdges).toSet
  }

  def inducedEdges(vs: Iterable[Vertex]): Set[Edge] = {
    assert(vs.toSet subsetOf vertices)
    val vsSet = vs.toSet
    edges.filter(e => (vsSet contains e.in) && (vsSet contains e.out))
  }

  def incidentNonTerminals(v: Vertex): List[NonTerminal] = {
    assert(vertices contains v)
    nonTerminals.filter(_ contains v)
  }
  def incidentNonTerminals(vs: Iterable[Vertex]): List[NonTerminal] = {
    assert(vs.toSet subsetOf vertices)
    val vsSet = vs.toSet
    nonTerminals.filter(_.connectors.exists(vsSet contains _))
  }
  def inducedNonTerminals(vs: Iterable[Vertex]): List[NonTerminal] = {
    assert(vs.toSet subsetOf vertices)
    val vsSet = vs.toSet
    nonTerminals.filter(_.connectors.toSet subsetOf vsSet)
  }

  def inducedSubGraph(vs: Iterable[Vertex]): Graph[V, E] = {
    assert(vs.toSet subsetOf vertices)
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

  def replaceOne[V1 >: V, E1 >: E](nonTerminal: NonTerminal, replacement: Graph[V1, E1], autoId: AutoId): Graph[V1, E1] = {
    assert(this.nonTerminals contains nonTerminal)
    assert(nonTerminal.connectors.size == replacement.connectors.size)

    // newly created vertices that will be merged into the graph at fringe vertices
    val newVertices = (replacement.vertices -- replacement.connectors).map(_.label -> Vertex(autoId.nextId)).toMap
    // existing fringe/connectivity vertices for merge process
    val existVertices = replacement.connectors.map(_.label).zip(nonTerminal.connectors).toMap
    val vertexMap: Map[Label, Vertex] = newVertices ++ existVertices

    val vertices = replacement.vertices.map(v => vertexMap(v.label))

    val edges = replacement.edges.map { case Edge(Vertex(in), Vertex(out)) => Edge(vertexMap(in), vertexMap(out)) }

    val nonTerminals = replacement.nonTerminals.map { case NonTerminal(label, connectors) => NonTerminal(label, connectors.map(v => vertexMap(v.label))) }

    val vertexData = replacement.vertexData.map { case (Vertex(label), v) => vertexMap(label) -> v }.toMap
    val edgeData = replacement.edgeData.map { case (Edge(Vertex(in), Vertex(out)), v) => Edge(vertexMap(in), vertexMap(out)) -> v }.toMap

    Graph(
      this.vertices ++ vertices,
      this.edges ++ edges,
      this.vertexData ++ vertexData,
      this.edgeData ++ edgeData,
      (this.nonTerminals diff List(nonTerminal)) ++ nonTerminals, // diff does not remove all instances
      this.connectors
    )
  }

  override def toString = s"Graph(V(${vertices.toList.sortBy(_.label).mkString(" ")}), " +
    s"E(${edges.toList.sortBy(_.out.label).sortBy(_.in.label).mkString(", ")})" +
    (if (vertexData.nonEmpty) s", {${vertexData.toList.sortBy(_._1.label).map{ case (v, d) => s"$v: $d" }.mkString(", ")}}" else "") +
    (if (edgeData.nonEmpty) s", {${edgeData.toList.sortBy(_._1.out.label).sortBy(_._1.in.label).map{ case (Edge(in, out), d) => s"$in->$out: $d" }.mkString(", ")}}" else "") +
    (if (nonTerminals.nonEmpty) s", NTS(${nonTerminals.mkString(", ")})" else "") +
    (if (connectors.nonEmpty) s", C(${connectors.mkString("-")})" else "") +
    ")"

}
