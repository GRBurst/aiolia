package aiolia.graph

import aiolia.util.AutoId

import scala.collection.mutable

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
) extends DirectedGraphLike {

  assert(connectors == connectors.distinct, "connectors in graph need to be distinct")
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only be attached to existing vertices")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only be attached to existing edges")
  assert(nonTerminals.flatMap(_.connectors).toSet subsetOf vertices, "NonTerminals can only connect existing vertices")
  assert(nonTerminals.groupBy(_.label).values.forall(_.map(_.connectors.size).distinct.size == 1), "NonTerminals with same label must have the same number of connectors")
  assert(connectors.toSet subsetOf vertices, s"Only existing vertices can be used as connectors. (Connectors = $connectors not subset of Vertices = $vertices)")
  assert((vertexData.keySet intersect connectors.toSet).isEmpty, "Connectors cannot store data")

  def nonConnectors = vertices -- connectors
  def allIsolatedVertices = vertices.filter(allDegree(_) == 0)

  def subGraphOf[V1 >: V, E1 >: E](superGraph: Graph[V1, E1]) = {
    (this.vertices subsetOf superGraph.vertices) &&
      (this.edges subsetOf superGraph.edges) &&
      (this.nonTerminals.toSet subsetOf superGraph.nonTerminals.toSet) &&
      // (this.connectors.size <= superGraph.connectors.size) && //TODO: performance!
      ((superGraph.connectors intersect this.connectors) == this.connectors)
  }

  def nonTerminalDegree(v: Vertex) = neighboursOverNonTerminals(v).size
  def allDegree(v: Vertex) = degree(v) + nonTerminalDegree(v)

  def +(v: Vertex) = {
    assert(!(vertices contains v), s"Vertex $v already exists in $vertices")
    copy(vertices = vertices + v)
  }

  def +(e: Edge) = {
    assert(!(edges contains e), s"Edge $e already exists in $edges")
    copy(edges = edges + e)
  }

  def +(nt: NonTerminal) = copy(nonTerminals = nt :: nonTerminals)

  def -(v: Vertex) = {
    assert(vertices contains v, s"Vertex $v does not exist in $vertices")
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
    assert(edges contains e, s"Edge $e does not exist in $edges")
    copy(edges = edges - e, edgeData = edgeData - e)
  }

  def -(nt: NonTerminal) = {
    assert(nonTerminals contains nt, s"NonTerminal $nt does not exist in $nonTerminals")
    val i = nonTerminals indexOf nt
    copy(nonTerminals = nonTerminals.take(i) ++ nonTerminals.drop(i + 1))
  }

  def neighboursOverNonTerminals(v: Vertex): Set[Vertex] = {
    assert(vertices contains v)
    nonTerminals.flatMap{
      case NonTerminal(_, connectors) if connectors contains v => connectors
      case _ => Nil
    }.toSet - v
  }

  def neighboursOverNonTerminals(vp: (Vertex) => Boolean): Set[Vertex] = {
    vertices.filter(vp).flatMap(neighboursOverNonTerminals).filterNot(vp)
  }

  def allNeighbours(v: Vertex) = neighbours(v) ++ neighboursOverNonTerminals(v)
  def allNeighbours(vp: (Vertex) => Boolean) = neighbours(vp) ++ neighboursOverNonTerminals(vp)

  def incidentNonTerminals(v: Vertex): List[NonTerminal] = {
    assert(vertices contains v)
    nonTerminals.filter(_ contains v)
  }
  def incidentNonTerminals(vp: (Vertex) => Boolean): List[NonTerminal] = {
    nonTerminals.filter(_.connectors.exists(vp))
  }
  def inducedNonTerminals(vp: (Vertex) => Boolean): List[NonTerminal] = {
    nonTerminals.filter(_.connectors.forall(vp))
  }

  def inducedSubGraph(vp: (Vertex) => Boolean): Graph[V, E] = {
    val selectedEdges = inducedEdges(vp)
    val subGraph = Graph(
      vertices.filter(vp),
      selectedEdges,
      vertexData filterKeys vp,
      edgeData filterKeys selectedEdges,
      inducedNonTerminals(vp),
      connectors filter vp
    )
    assert(subGraph subGraphOf this)
    subGraph
  }

  def --[E1, V1](subGraph: Graph[E1, V1]) = {
    //TODO: rething connectors in subGraphOf
    assert(subGraph.copy(connectors = Nil) subGraphOf this, s"Graph can only remove valid subgraph. $this -- $subGraph)")
    // assert(subGraph.connectors.isEmpty, "Graph connectors must not be empty")

    val removedVertices = subGraph.vertices
    val removedEdges = subGraph.edges ++ incidentEdges(subGraph.vertices)
    val removedNonTerminals = subGraph.nonTerminals ++ incidentNonTerminals(subGraph.vertices)

    Graph(
      vertices -- removedVertices,
      edges -- removedEdges,
      vertexData -- removedVertices,
      edgeData -- removedEdges,
      nonTerminals diff removedNonTerminals,
      connectors filterNot removedVertices
    )
  }

  def --(vs: Iterable[Vertex]) = {
    //TODO: optimize
    vs.foldLeft(this)((graph, v) => graph - v)
  }

  def removeEdges(es: Iterable[Edge]) = {
    //TODO: optimize
    es.foldLeft(this)((graph, e) => graph - e)
  }

  def removeNonTerminals(vs: Iterable[NonTerminal]) = {
    //TODO: optimize
    vs.foldLeft(this)((graph, v) => graph - v)
  }

  def ++[V1 >: V, E1 >: E](that: Graph[V1, E1]): Graph[V1, E1] = {
    assert(that.connectors.isEmpty, "Now think about it. What should happen with the connectors?")
    Graph(
      this.vertices ++ that.vertices,
      this.edges ++ that.edges,
      this.vertexData ++ that.vertexData,
      this.edgeData ++ that.edgeData,
      this.nonTerminals ::: that.nonTerminals,
      this.connectors //TODO: think
    )
  }

  def map(m: Label => Label): Graph[V, E] = {
    Graph(
      vertices map (_ map m),
      edges map (_ map m),
      vertexData map { case (v, d) => (v map m) -> d },
      edgeData map { case (e, d) => (e map m) -> d },
      nonTerminals map (_ map m),
      connectors map (_ map m)
    )
  }

  def replaceOne[V1 >: V, E1 >: E](nonTerminal: NonTerminal, replacement: Graph[V1, E1], autoId: AutoId): Graph[V1, E1] = {
    //TODO: optimization: replace first nonTerminal in graph
    // we have to remove the nonTerminal from graph, instead we could just use this.nonTerminals.tail
    assert(this.nonTerminals contains nonTerminal)
    assert(nonTerminal.connectors.size == replacement.connectors.size)

    def autoIdFor(v: Vertex): Label = autoId.nextId
    // def autoIdFor(v: Vertex):Label = if(this.vertices.contains(v)) autoId.nextId else { autoId.setIfHigher(v.label); v.label }

    val connectorMapping = (replacement.connectors.map(_.label) zip nonTerminal.connectors.map(_.label)).toMap
    val nonConnectorMapping = replacement.nonConnectors.map(v => v.label -> autoIdFor(v)).toMap
    val vertexMapping: Map[Label, Label] = nonConnectorMapping ++ connectorMapping

    assert(vertexMapping.size == replacement.vertices.size)
    val mappedReplacement = replacement.copy(connectors = Nil) map vertexMapping

    this.copy(nonTerminals = this.nonTerminals diff List(nonTerminal)) ++ mappedReplacement
  }

  override def toString = s"G(V(${vertices.toList.sortBy(_.label).mkString(", ")}), " +
    s"E(${edges.toList.sortBy(_.out.label).sortBy(_.in.label).mkString(", ")})" +
    (if (vertexData.nonEmpty) s", {${vertexData.toList.sortBy(_._1.label).map{ case (v, d) => s"$v: $d" }.mkString(", ")}}" else "") +
    (if (edgeData.nonEmpty) s", {${edgeData.toList.sortBy(_._1.out.label).sortBy(_._1.in.label).map{ case (Edge(in, out), d) => s"$in->$out: $d" }.mkString(", ")}}" else "") +
    (if (nonTerminals.nonEmpty) s", NTS(${nonTerminals.sortBy(_.connectors.headOption.map(_.label)).sortBy(_.label).mkString(", ")})" else "") +
    (if (connectors.nonEmpty) s", C(${connectors.mkString("-")})" else "") +
    ")"
}
