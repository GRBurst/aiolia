package aiolia.graph

package object types {
  type Label = Int
}

import types._
import collection.mutable

case class Vertex(label: Label) {
  override def toString = s"($label)"
}

case class Edge(in: Vertex, out: Vertex) {
  override def toString = s"$in -> $out"
}

case class Graph[+E, +V](vertices: Set[Vertex], edges: Set[Edge], vertexData: Map[Vertex, V] = Map.empty[Vertex, V], edgeData: Map[Edge, E] = Map.empty[Edge, E]) {

  assert(edges.flatMap(e => Seq(e.in, e.out)).forall(vertices contains _), "All vertices used in edges have to be defined")

  // def successors(v: Vertex) = edges.collect { case Edge(`v`, o) => o }
  lazy val successors:Map[Vertex, Set[Vertex]] = edges.foldLeft(Map.empty[Vertex, Set[Vertex]].withDefaultValue(Set.empty)){case (suc, Edge(in,out)) => suc + (in -> (suc(in) + out)) }
  // def predecessors(v: Vertex) = edges.collect { case Edge(i, `v`) => i }
  lazy val predecessors:Map[Vertex, Set[Vertex]] = edges.foldLeft(Map.empty[Vertex, Set[Vertex]].withDefaultValue(Set.empty)){case (pre, Edge(in,out)) => pre + (out -> (pre(out) + in)) }
  // def incomingEdges(v: Vertex) = edges.filter(_.out == v)
  lazy val incomingEdges:Map[Vertex, Set[Edge]] = edges.foldLeft(Map.empty[Vertex, Set[Edge]].withDefaultValue(Set.empty)){case (incoming, edge@Edge(_,out)) => incoming + (out -> (incoming(out) + edge)) }
  // def outgoingEdges(v: Vertex) = edges.filter(_.in == v)
  lazy val outgoingEdges:Map[Vertex, Set[Edge]] = edges.foldLeft(Map.empty[Vertex, Set[Edge]].withDefaultValue(Set.empty)){case (outgoing, edge@Edge(in,_)) => outgoing + (in -> (outgoing(in) + edge)) }

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


  override def toString = s"Graph(0 to ${vertices.size - 1}, $edges, $vertexData, $edgeData)"
}

