package aiolia.graph

package object types {
  type Label = Int
}

import types._
import collection.mutable

case class Vertex(label: Label) {
  override def toString = s"$label"
}

case class Edge(in: Vertex, out: Vertex) {
  def contains(v: Vertex) = in == v || out == v
  override def toString = s"${in.label} -> ${out.label}"
}

case class Graph[+E, +V](vertices: Set[Vertex] = Set.empty, edges: Set[Edge] = Set.empty, vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {

  //TODO: allow selfloops?
  assert(edges.flatMap(e => List(e.in, e.out)).forall(vertices contains _), "All vertices used in edges have to be defined")
  assert(vertexData.keys.toSet.diff(vertices).isEmpty, "Vertex data can only contain vertices in Graph")
  assert(edgeData.keys.toSet.diff(edges).isEmpty, "Edge data can only contain edges in Graph")

  // def successors(v: Vertex) = edges.collect { case Edge(`v`, out) => out }
  lazy val successors:Map[Vertex, Set[Vertex]] = edges.foldLeft(Map.empty[Vertex, Set[Vertex]].withDefaultValue(Set.empty)){case (suc, Edge(in,out)) => suc + (in -> (suc(in) + out)) }
  // def predecessors(v: Vertex) = edges.collect { case Edge(in, `v`) => in }
  lazy val predecessors:Map[Vertex, Set[Vertex]] = edges.foldLeft(Map.empty[Vertex, Set[Vertex]].withDefaultValue(Set.empty)){case (pre, Edge(in,out)) => pre + (out -> (pre(out) + in)) }
  // def incomingEdges(v: Vertex) = edges.filter(_.out == v)
  lazy val incomingEdges:Map[Vertex, Set[Edge]] = edges.foldLeft(Map.empty[Vertex, Set[Edge]].withDefaultValue(Set.empty)){case (incoming, edge@Edge(_,out)) => incoming + (out -> (incoming(out) + edge)) }
  // def outgoingEdges(v: Vertex) = edges.filter(_.in == v)
  lazy val outgoingEdges:Map[Vertex, Set[Edge]] = edges.foldLeft(Map.empty[Vertex, Set[Edge]].withDefaultValue(Set.empty)){case (outgoing, edge@Edge(in,_)) => outgoing + (in -> (outgoing(in) + edge)) }

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

  def hasCycle:Boolean = {
    val next = mutable.HashSet.empty ++ vertices

    while( next.nonEmpty ) {
      if( cycleAt(next.head) ) return true
    }

    def cycleAt(v: Vertex, visited: Set[Vertex] = Set.empty):Boolean = {
      if(visited contains v) return true // found cycle
      if(!(next contains v)) return false // we already checked from here, there is definitely no cycle

      next -= v
      successors(v).exists(cycleAt(_, visited + v))
    }

    return false
  }


  override def toString = s"Graph([${vertices.toList.sortBy(_.label).mkString(" ")}], " +
                          s"[${edges.toList.sortBy(_.out.label).sortBy(_.in.label).mkString(", ")}]" +
                          (if(vertexData.nonEmpty) s", {${vertexData.toList.sortBy(_._1.label).map{case (v,d) => s"$v: $d"}.mkString(", ")}}" else "") +
                          (if(edgeData.nonEmpty) s", {${edgeData.toList.sortBy(_._1.out.label).sortBy(_._1.in.label).map{case (Edge(in,out),d) => s"$in->$out: $d"}.mkString(", ")}}" else "") +
                          ")"

}

