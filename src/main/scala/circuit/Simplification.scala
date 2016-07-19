package aiolia.circuit

import aiolia.graph._
import aiolia.util.{DOTExport, _}
import scala.util.Try

object Types {
  type NANDLogic = Graph[Nothing, Nothing]
}
import Types._

object Simplify extends Simplification {
  val simplifications: List[Simplification] = List(RemoveDoubleInversion, MergeIdenticalSuccessors)

  def simplify(in: List[Vertex], out: List[Vertex], graph: NANDLogic): NANDLogic = {
    simplifications.toStream.flatMap(s => s(in, out, graph)).headOption match {
      case Some(simplifiedGraph) =>
        assert((simplifiedGraph.vertices intersect in.toSet) == in.toSet)
        assert((simplifiedGraph.vertices intersect out.toSet) == out.toSet)
        // println(s"found simplification: ${simplifiedGraph.vertices.size}")
        // File.write("/tmp/currentgraph.dot", DOTExport.toDOT(graph, in, out))
        // File.write("/tmp/currentgraph1.dot", DOTExport.toDOT(simplifiedGraph, in, out))
        assert { Circuit(in, out, remapVertexLabels(in, out, simplifiedGraph)); true }
        simplify(in, out, simplifiedGraph)
      case None =>
        // println("remapping vertices")
        // println(graph)
        val ng = remapVertexLabels(in, out, graph)
        assert { Circuit(in, out, ng); true }
        ng
    }
  }

  def apply(in: List[Vertex], out: List[Vertex], graph: NANDLogic): Option[NANDLogic] = {
    val simplified = simplify(in, out, graph)
    if (simplified == graph)
      None
    else
      Some(simplified)
  }

  private def nextLabel(it: Iterable[Vertex]) = Try(it.maxBy(_.label).label + 1).getOrElse(0)
  def remapVertexLabels(outputs: Seq[Vertex], inputs: Seq[Vertex], graph: NANDLogic): NANDLogic = {
    import graph._
    val autoId = AutoId(nextLabel(inputs ++ outputs))
    val vertexMap = ((vertices -- inputs -- outputs).map(_.label) zip autoId).toMap.withDefault(k => k)
    graph mapVertices vertexMap
  }

  def removeUnusedCircuits(inputs: Seq[Vertex], outputs: Seq[Vertex], graph: NANDLogic): NANDLogic = {
    import graph._
    val keepVertices = (outputs ++ inputs ++ outputs.flatMap(depthFirstSearch(_, predecessors))).toSet
    val removeVertices = vertices -- keepVertices
    graph -- removeVertices
  }
}

case class GraphChange(
  addedVertices: Iterable[Vertex] = Nil,
  removedVertices: Iterable[Vertex] = Nil,
  addedEdges: Iterable[Edge] = Nil,
  removedEdges: Iterable[Edge] = Nil
) {
  def apply(graph: NANDLogic): NANDLogic = {
    import graph._
    assert((vertices intersect addedVertices.toSet).isEmpty)
    graph.copy(
      vertices = vertices -- removedVertices ++ addedVertices,
      edges = edges -- removedEdges -- incidentEdges(removedVertices.toSet) ++ addedEdges
    )
  }
}

trait Simplification {
  def apply(in: List[Vertex], out: List[Vertex], graph: NANDLogic): Option[NANDLogic]
}

object RemoveDoubleInversion extends Simplification {
  def apply(in: List[Vertex], out: List[Vertex], graph: NANDLogic): Option[NANDLogic] = {
    import graph._
    def isInverter(v: Vertex): Boolean = inDegree(v) == 1
    val changes = (for (
      in <- vertices.toStream;
      inv1 <- successors(in) if isInverter(inv1);
      inv2 <- successors(inv1) if isInverter(inv2);
      out <- successors(inv2) if outDegree(inv1) == 1 || outDegree(inv2) == 1
    ) yield {
      // println(s"double inversion: $in, $inv1, $inv2, $out:\n$graph")
      if (outDegree(inv1) == 1 && outDegree(inv2) == 1) {
        GraphChange(
          removedVertices = Set(inv1, inv2),
          addedEdges = Edge(in, out) :: Nil
        )
      } else if (outDegree(inv2) == 1) {
        assert(outDegree(inv1) > 1)
        GraphChange(
          removedVertices = Set(inv2),
          addedEdges = Edge(inv1, out) :: Edge(in, out) :: Nil
        )
      } else {
        assert(outDegree(inv1) == 1)
        assert(outDegree(inv2) > 1)
        GraphChange(
          removedVertices = Set(inv1, inv2),
          addedEdges = successors(inv2).map(o => Edge(in, o))
        )
      }
    }).headOption
    changes.map(_.apply(graph))
  }
}

object MergeIdenticalSuccessors extends Simplification {
  def apply(in: List[Vertex], out: List[Vertex], graph: NANDLogic): Option[NANDLogic] = {
    import graph._

    // (parentA, parentB) --> childX
    // (parentA, parentB) --> childY
    // remove childY, move all successors to childX

    val changes = (for (
      parentA <- vertices.toStream;
      childX <- successors(parentA) -- out if inDegree(childX) == 2;
      parentB <- predecessors(childX) - parentA;
      childY <- successors(parentB) -- out - childX - parentA if inDegree(childY) == 2 && (predecessors(childY) contains parentA)
    ) yield {
      // println(s"identical successors: $parentA, $parentB, $childX, $childY:\n$graph")
      GraphChange(
        removedVertices = childY :: Nil,
        addedEdges = (successors(childY) - childX).map(v => Edge(childX, v))
      )
    }).headOption

    changes.map(_.apply(graph))
  }
}
