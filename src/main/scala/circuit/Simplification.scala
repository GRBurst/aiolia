package aiolia.circuit

import aiolia.graph._
import aiolia.util.{DOTExport, _}
import scala.util.Try

object Simplification {
  type NANDLogic = Graph[Nothing, Nothing]
  private def nextLabel(it: Iterable[Vertex]) = Try(it.maxBy(_.label).label + 1).getOrElse(0)
  def removeDoubleInversion(graph: NANDLogic): NANDLogic = {

    // in --> inv1 --> inv2 --> out
    // in --> out

    // File.write("/tmp/currentgraph.dot", DOTExport.toDOT(graph))
    def isInverter(v: Vertex, g: NANDLogic): Boolean = g.inDegree(v) == 1

    var currentGraph = graph

    def findRemovableDoubleInversion(currentGraph: NANDLogic) = (for (
      in <- currentGraph.vertices;
      inv1 <- currentGraph.successors(in) if isInverter(inv1, currentGraph);
      inv2 <- currentGraph.successors(inv1) if isInverter(inv2, currentGraph);
      out <- currentGraph.successors(inv2) if currentGraph.outDegree(inv1) == 1 || currentGraph.outDegree(inv2) == 1
    ) yield {
      // println(s"double inversion: $in, $inv1, $inv2, $out:\n$graph")
      (inv1, inv2, Edge(in, out))
    }).headOption

    var doubleInversion = findRemovableDoubleInversion(currentGraph)
    while (doubleInversion.isDefined) {
      val Some((inv1, inv2, addedEdge)) = doubleInversion
      val removedInverters = Set(inv1, inv2).filter(currentGraph.outDegree(_) == 1)

      // println(s"removing: $removedInverters, adding: $addedEdge")
      currentGraph = currentGraph.copy(
        vertices = currentGraph.vertices -- removedInverters,
        edges = currentGraph.edges -- currentGraph.incidentEdges(removedInverters) + addedEdge
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
