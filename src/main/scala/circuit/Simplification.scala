package aiolia.circuit

import aiolia.graph._
import aiolia.util.{DOTExport, _}
import scala.util.Try

object Simplification {
  type NANDLogic = Graph[Nothing, Nothing]
  private def nextLabel(it: Iterable[Vertex]) = Try(it.maxBy(_.label).label + 1).getOrElse(0)

  def simplify(in: List[Vertex], out: List[Vertex], graph: NANDLogic): NANDLogic = {
    remapVertexLabels(in, out, removeDoubleInversion(graph))
  }

  def removeDoubleInversion(graph: NANDLogic): NANDLogic = {

    // in --> inv1 --> inv2 --> out
    // in --> out

    // File.write("/tmp/currentgraph.dot", DOTExport.toDOT(graph))
    def isInverter(v: Vertex, g: NANDLogic): Boolean = g.inDegree(v) == 1

    def findRemovableDoubleInversion(currentGraph: NANDLogic) = {
      import currentGraph._
      (for (
        in <- vertices.toStream;
        inv1 <- successors(in) if isInverter(inv1, currentGraph);
        inv2 <- successors(inv1) if isInverter(inv2, currentGraph);
        out <- successors(inv2) if outDegree(inv1) == 1 || outDegree(inv2) == 1
      ) yield {
        // println(s"double inversion: $in, $inv1, $inv2, $out:\n$graph")
        if (outDegree(inv1) == 1 && outDegree(inv2) == 1) {
          (Set(inv1, inv2), Edge(in, out) :: Nil)
        } else if (outDegree(inv2) == 1) {
          assert(outDegree(inv1) > 1)
          (Set(inv2), Edge(inv1, out) :: Edge(in, out) :: Nil)
        } else {
          assert(outDegree(inv1) == 1)
          assert(outDegree(inv2) > 1)
          (Set(inv1, inv2), successors(inv2).map(o => Edge(in, o)))
        }
      }).headOption
    }

    var currentGraph = graph
    var doubleInversion = findRemovableDoubleInversion(currentGraph)
    while (doubleInversion.isDefined) {
      val g = currentGraph
      import g._

      val Some((removedInverters, addedEdges)) = doubleInversion
      // println(s"removing: $removedInverters, adding: $addedEdges")
      currentGraph = copy(
        vertices = vertices -- removedInverters,
        edges = edges -- incidentEdges(removedInverters) ++ addedEdges
      )
      doubleInversion = findRemovableDoubleInversion(currentGraph)
    }

    // File.write("/tmp/currentgraph1.dot", DOTExport.toDOT(currentGraph))
    // if (removedInverters.nonEmpty)
    //   System.exit(0)
    currentGraph
  }

  def removeIdenticalSuccessors(graph: NANDLogic): NANDLogic = {
    import graph._
    // (parentA, parentB) --> childX
    // (parentA, parentB) --> childY
    // remove childY connect all successors to start at childX

    for (
      parentA <- vertices;
      childX <- successors(parentA);
      parentB <- predecessors(childX) - parentA;
      childY <- successors(parentB) - childX
    ) {
      println(s"identical successors: $parentA, $parentB, $childX, $childY:\n$graph")
      File.write("/tmp/currentgraph.dot", DOTExport.toDOT(graph))
      System.exit(0)
      // File.write("/tmp/currentgraph.dot", DOTExport.toDOT(graph))
    }

    graph
  }

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
