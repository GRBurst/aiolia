package aiolia.graph

import scala.collection.mutable

trait DirectedGraphLike {
  def vertices: Set[Vertex]
  def edges: Set[Edge]

  assert(edges.flatMap(e => List(e.in, e.out)) subsetOf vertices, "Edges can only connect existing vertices")

  def isolatedVertices = vertices.filter(degree(_) == 0)
  def inDegree(v: Vertex) = predecessors(v).size
  def outDegree(v: Vertex) = successors(v).size
  def degree(v: Vertex) = inDegree(v) + outDegree(v)
  def numElements = vertices.size + edges.size

  // lazy caching datastructure for successors, predecessors, incomingEdges, outgoingEdges
  private def MapVVempty = Map.empty[Vertex, Set[Vertex]].withDefault((v: Vertex) => { assert(vertices contains v); Set.empty[Vertex] })
  private def MapVEempty = Map.empty[Vertex, Set[Edge]].withDefault((v: Vertex) => { assert(vertices contains v); Set.empty[Edge] })

  lazy val successors: Map[Vertex, Set[Vertex]] = edges.foldLeft(MapVVempty){ case (suc, Edge(in, out)) => suc + (in -> (suc(in) + out)) }
  lazy val predecessors: Map[Vertex, Set[Vertex]] = edges.foldLeft(MapVVempty){ case (pre, Edge(in, out)) => pre + (out -> (pre(out) + in)) }
  lazy val incomingEdges: Map[Vertex, Set[Edge]] = edges.foldLeft(MapVEempty){ case (incoming, edge @ Edge(_, out)) => incoming + (out -> (incoming(out) + edge)) }
  lazy val outgoingEdges: Map[Vertex, Set[Edge]] = edges.foldLeft(MapVEempty){ case (outgoing, edge @ Edge(in, _)) => outgoing + (in -> (outgoing(in) + edge)) }
  // def successors(v: Vertex) = { assert(vertices contains v); edges.collect { case Edge(`v`, out) => out } }
  // def predecessors(v: Vertex) = { assert(vertices contains v); edges.collect { case Edge(in, `v`) => in } }
  // def outgoingEdges(v: Vertex) = { assert(vertices contains v); edges.filter(_.in == v) }
  // def incomingEdges(v: Vertex) = { assert(vertices contains v); edges.filter(_.out == v) }

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

  def topologicalSort: List[Vertex] = {
    assert(!hasCycle)

    var sorted: List[Vertex] = Nil
    val unmarked = mutable.HashSet.empty[Vertex] ++ vertices
    val tempMarked = mutable.HashSet.empty[Vertex]

    while (unmarked.nonEmpty) visit(unmarked.head)

    def visit(n: Vertex) {
      if (unmarked(n)) {
        tempMarked += n
        for (m <- successors(n)) visit(m)
        unmarked -= n
        tempMarked -= n
        sorted ::= n
      }
    }

    sorted
  }

  def depthFirstSearch(start: Vertex, continue: Vertex => Iterable[Vertex] = successors) = new Iterator[Vertex] {
    //TODO: optimization use linear time algorithm
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

  def isEmpty = vertices.isEmpty

  //TODO: rename? undirected case with n*(n-1)/2?
  def isComplete = {
    val n = vertices.size
    edges.size == n * (n - 1)
  }

  def connectedComponents: Set[Set[Vertex]] = connectedComponents(neighbours)
  def connectedComponents(neighbourSelector: Vertex => Iterable[Vertex]): Set[Set[Vertex]] = {
    val toVisit = mutable.HashSet.empty ++ vertices
    val components = mutable.HashSet.empty[Set[Vertex]]
    while (toVisit.nonEmpty) {
      val start = toVisit.head
      val component = depthFirstSearch(start, neighbourSelector).toSet
      components += component
      toVisit --= component
    }
    components.toSet
  }

  //TODO: optimization: isComplete || depthFirst...
  def isConnected = isEmpty || depthFirstSearch(vertices.head, neighbours).size == vertices.size

  def hasCycle: Boolean = {
    //TODO: optimization: use linear time algorithm
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

    false
  }

  def isIsomorphicTo(that: DirectedGraphLike): Boolean = {
    if (this.vertices.size != that.vertices.size) return false
    if (this.edges.size != that.edges.size) return false
    if (this.vertices.toList.map(this.degree).sorted != that.vertices.toList.map(that.degree).sorted) return false

    if (this.vertices.size > 20) println(s"Isomorphism testing on Graph with ${this.vertices.size} vertices...")

    val thisLabels = this.vertices.map(_.label)
    val thatLabels = that.vertices.map(_.label)

    def prune(candA: Label, candB: Label, perm: Map[Label, Label]) = {
      val (vA, vB) = (Vertex(candA), Vertex(candB))
      this.inDegree(vA) != that.inDegree(vB) ||
        this.outDegree(vA) != that.outDegree(vB) ||
        perm.toList.exists{ case (a, b) => this.edges.contains(Edge(Vertex(a), vA)) != that.edges.contains(Edge(Vertex(b), vB)) } ||
        perm.toList.exists{ case (a, b) => this.edges.contains(Edge(vA, Vertex(a))) != that.edges.contains(Edge(vB, Vertex(b))) }
      // !(this.inducedSubGraph((thisLabels -- perm.keySet - candA).map(Vertex(_))) isIsomorphicTo that.inducedSubGraph((thatLabels -- perm.values - candB).map(Vertex(_))))
    }

    def recurse(perm: Map[Label, Label]): Boolean = {
      if (perm.size == this.vertices.size) {
        vertices.map (_ map perm) == that.vertices &&
          edges.map (_ map perm) == that.edges
      }
      else {
        val thisCandidates = thisLabels -- perm.keySet
        val thatCandidates = thatLabels -- perm.values
        val thisCandidate = thisCandidates.head
        thatCandidates.filterNot(prune(thisCandidate, _, perm)).exists { thatCandidate =>
          recurse(perm + (thisCandidate -> thatCandidate))
        }
      }
    }

    recurse(Map.empty)
  }
}
