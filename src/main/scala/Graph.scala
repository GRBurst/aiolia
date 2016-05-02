package aiolia.graph

import aiolia.helpers.AutoId

object types {
  type Label = Int
}

object dsl {
  import types._
  //TODO: graph dsl: LinkedHashSet for predictable vertex/edge traversal in tests
  //TODO: graph dsl: G as alias for Graph
  //TODO: implicits for accessors: V,E,NT,C, ....

  // Graph construction
  def v(label: Label) = Vertex(label)
  def V(labels: Range) = labels.map(Vertex(_)).toSet
  def V(labels: Label*) = labels.map(Vertex(_)).toSet
  def VL(labels: Label*) = labels.map(Vertex(_)).toList
  def e(edge: (Label, Label)) = edge match { case (in, out) => Edge(Vertex(in), Vertex(out)) }
  def E(es: (Label, Label)*) = es.map{ case (in, out) => Edge(Vertex(in), Vertex(out)) }.toSet
  def C(labels: Label*) = labels.map(Vertex(_)).toList // connectors

  def vData[V](data: (Int, V)*) = data.map { case (label, datum) => Vertex(label) -> datum }.toMap
  def eData[E](data: ((Int, Int), E)*) = data.map { case ((a, b), datum) => e(a -> b) -> datum }.toMap

  // NonTerminal
  def nt(l: Label, c: Product = None) = {
    assert(!c.isInstanceOf[List[_]], s"don't put Lists in NT($l, $c). Use a Tuple instead: NT($l, ${c.asInstanceOf[List[_]].mkString("(", ",", ")")})")
    NonTerminal(l, c.productIterator.toList.asInstanceOf[List[Int]].map(Vertex(_)))
  }
}

case object IsotopicException extends Exception

import types._
import collection.mutable

//TODO: make Vertex a value class? (problems with mockito)
// case class Vertex(label: Label) extends AnyVal {
case class Vertex(label: Label) {
  override def toString = s"$label"
  def map(vm: Label => Label) = Vertex(vm(label))
}

case class Edge(in: Vertex, out: Vertex) {
  assert(in != out, "Self loops are not allowed")
  def contains(v: Vertex) = in == v || out == v
  override def toString = s"${in.label} -> ${out.label}"
  def toSet = Set(in, out)
  def map(vm: Label => Label) = Edge(in map vm, out map vm)
}

case class NonTerminal(label: Label, connectors: List[Vertex] = Nil) {
  assert(connectors == connectors.distinct, "connectors in graph need to be distinct")

  def contains(v: Vertex) = connectors contains v
  override def toString = s"[$label${if (connectors.nonEmpty) s":${connectors.mkString("-")}" else ""}]"
  def map(vm: Label => Label) = NonTerminal(label, connectors map (_ map vm))
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

  assert(connectors == connectors.distinct, "connectors in graph need to be distinct")
  assert(edges.flatMap(e => List(e.in, e.out)) subsetOf vertices, "Edges can only connect existing vertices")
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only be attached to existing vertices")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only be attached to existing edges")
  assert(nonTerminals.flatMap(_.connectors).toSet subsetOf vertices, "NonTerminals can only connect existing vertices")
  assert(nonTerminals.groupBy(_.label).values.forall(_.map(_.connectors.size).distinct.size == 1), "NonTerminals with same label must have the same number of connectors")
  assert(connectors.toSet subsetOf vertices, s"Only existing vertices can be used as connectors. (Connectors = $connectors not subset of Vertices = $vertices)")
  assert((vertexData.keySet intersect connectors.toSet).isEmpty, "Connectors cannot store data")

  def nonConnectors = vertices -- connectors
  def isolatedVertices = vertices.filter(degree(_) == 0)
  def allIsolatedVertices = vertices.filter(allDegree(_) == 0)

  def subGraphOf[V1 >: V, E1 >: E](superGraph: Graph[V1, E1]) = {
    (this.vertices subsetOf superGraph.vertices) &&
      (this.edges subsetOf superGraph.edges) &&
      (this.nonTerminals.toSet subsetOf superGraph.nonTerminals.toSet) &&
      // (this.connectors.size <= superGraph.connectors.size) && //TODO: performance!
      ((superGraph.connectors intersect this.connectors) == this.connectors)
  }

  def successors(v: Vertex) = { assert(vertices contains v); edges.collect { case Edge(`v`, out) => out } }
  def predecessors(v: Vertex) = { assert(vertices contains v); edges.collect { case Edge(in, `v`) => in } }
  def outgoingEdges(v: Vertex) = { assert(vertices contains v); edges.filter(_.in == v) }
  def incomingEdges(v: Vertex) = { assert(vertices contains v); edges.filter(_.out == v) }

  def inDegree(v: Vertex) = predecessors(v).size
  def outDegree(v: Vertex) = successors(v).size
  def degree(v: Vertex) = inDegree(v) + outDegree(v)
  def nonTerminalDegree(v: Vertex) = neighboursOverNonTerminals(v).size
  def allDegree(v: Vertex) = degree(v) + nonTerminalDegree(v)

  // lazy caching datastructure for successors, predecessors, incomingEdges, outgoingEdges
  // private def MapVVempty = Map.empty[Vertex, Set[Vertex]].withDefault((v: Vertex) => { assert(vertices contains v); Set.empty[Vertex] })
  // private def MapVEempty = Map.empty[Vertex, Set[Edge]].withDefault((v: Vertex) => { assert(vertices contains v); Set.empty[Edge] })

  // lazy val successors: Map[Vertex, Set[Vertex]] = edges.foldLeft(MapVVempty){ case (suc, Edge(in, out)) => suc + (in -> (suc(in) + out)) }
  // lazy val predecessors: Map[Vertex, Set[Vertex]] = edges.foldLeft(MapVVempty){ case (pre, Edge(in, out)) => pre + (out -> (pre(out) + in)) }
  // lazy val incomingEdges: Map[Vertex, Set[Edge]] = edges.foldLeft(MapVEempty){ case (incoming, edge @ Edge(_, out)) => incoming + (out -> (incoming(out) + edge)) }
  // lazy val outgoingEdges: Map[Vertex, Set[Edge]] = edges.foldLeft(MapVEempty){ case (outgoing, edge @ Edge(in, _)) => outgoing + (in -> (outgoing(in) + edge)) }

  def +(v: Vertex) = {
    assert(!(vertices contains v), s"Vertex $v already exists in $vertices")
    copy(vertices = vertices + v)
  }

  def +(e: Edge) = {
    assert(!(edges contains e), s"Edge $e already exists in $edges")
    copy(edges = edges + e)
  }

  def +(h: NonTerminal) = copy(nonTerminals = h :: nonTerminals)

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

  def -(h: NonTerminal) = {
    assert(nonTerminals contains h, s"NonTerminal $h does not exist in $nonTerminals")
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
  def neighboursOverNonTerminals(v: Vertex): Set[Vertex] = {
    assert(vertices contains v)
    nonTerminals.flatMap{
      case NonTerminal(_, connectors) if (connectors contains v) => connectors
      case _ => Nil
    }.toSet - v
  }

  def neighboursOverNonTerminals(vs: Iterable[Vertex]): Set[Vertex] = {
    assert(vs.toSet subsetOf vertices)
    vs.map(neighboursOverNonTerminals).flatten.toSet -- vs
  }

  def allNeighbours(v: Vertex) = neighbours(v) ++ neighboursOverNonTerminals(v)
  def allNeighbours(vs: Iterable[Vertex]) = neighbours(vs) ++ neighboursOverNonTerminals(vs)

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
    nonTerminals.filter(_.connectors.exists(vsSet))
  }
  def inducedNonTerminals(vs: Iterable[Vertex]): List[NonTerminal] = {
    assert(vs.toSet subsetOf vertices)
    val vsSet = vs.toSet
    nonTerminals.filter(_.connectors.toSet subsetOf vsSet)
  }

  def inducedSubGraph(vs: Iterable[Vertex]): Graph[V, E] = {
    assert(vs.toSet subsetOf vertices, "Can only induce on existing vertices")

    val vsSet = vs.toSet
    val selectedEdges = inducedEdges(vsSet)
    val subGraph = Graph(
      vsSet,
      selectedEdges,
      vertexData filterKeys vsSet,
      edgeData filterKeys selectedEdges,
      inducedNonTerminals(vsSet),
      connectors filter vsSet
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

  def depthFirstSearch(start: Vertex, continue: Vertex => Iterable[Vertex] = successors) = new Iterator[Vertex] {
    assert(vertices contains start)
    val stack = mutable.Stack(start)
    val seen = mutable.Set[Vertex]()

    override def hasNext: Boolean = stack.nonEmpty
    override def next: Vertex = {
      val current = stack.pop
      seen += current
      stack pushAll (continue(current).toSeq diff (seen ++ stack).toSeq)
      current
    }
  }

  def isEmpty = vertices.size == 0

  //TODO: rename? undirected case with n*(n-1)/2?
  def isComplete = {
    val n = vertices.size
    edges.size == n * (n - 1)
  }

  //TODO: optimization: isComplete || depthFirst...
  def isConnected = isEmpty || depthFirstSearch(vertices.head, neighbours).size == vertices.size

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

  def isIsomorphicTo[V1, E1](that: Graph[V1, E1]): Boolean = {
    if (this.vertices.size != that.vertices.size) return false
    val thisLabels = this.vertices.map(_.label)
    val thatLabels = that.vertices.map(_.label)

    def recurse(perm: Map[Label, Label]): Boolean = {
      if (perm.size == this.vertices.size) {
        println((this, perm, that, (this map perm) == that))
        (this map perm) == that
      }
      else {
        val thisCandidates = thisLabels -- perm.keySet
        val thatCandidates = thatLabels -- perm.values
        recurse(perm + (thisCandidates.head -> thatCandidates.head))
      }
    }

    recurse(Map.empty)
  }

  override def toString = s"G(V(${vertices.toList.sortBy(_.label).mkString(", ")}), " +
    s"E(${edges.toList.sortBy(_.out.label).sortBy(_.in.label).mkString(", ")})" +
    (if (vertexData.nonEmpty) s", {${vertexData.toList.sortBy(_._1.label).map{ case (v, d) => s"$v: $d" }.mkString(", ")}}" else "") +
    (if (edgeData.nonEmpty) s", {${edgeData.toList.sortBy(_._1.out.label).sortBy(_._1.in.label).map{ case (Edge(in, out), d) => s"$in->$out: $d" }.mkString(", ")}}" else "") +
    (if (nonTerminals.nonEmpty) s", NTS(${nonTerminals.sortBy(_.connectors.headOption.map(_.label)).sortBy(_.label).mkString(", ")})" else "") +
    (if (connectors.nonEmpty) s", C(${connectors.mkString("-")})" else "") +
    ")"
}
