package aiolia.mutations

import aiolia.{Grammar, MutationOpConfig}
import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import util.Try

// TODO: Mutate is still not perfectly deterministic!
// This can happen when iterating over HashSets, MashMaps ...

trait MutationOp {
  def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]): Option[Grammar[V, E]]

  // helpers
  def nextLabel(it: Iterable[{ val label: Label }]) = Try(it.maxBy(_.label).label + 1).getOrElse(0)
}

case class MutationAnd(ops: List[MutationOp]) extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    val opt: Option[Grammar[V,E]] = Some(grammar)
    ops.foldLeft(opt) ((gOpt, op) => gOpt.flatMap(g => op(g, config)))
  }
}
object MutationAnd {
  def fill(ops: List[(Int, MutationOp)]): MutationAnd = MutationAnd(ops.flatMap { case (n, op) => List.fill(n)(op) })
}

case class MutationOr(ops: List[MutationOp]) extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    val op = config.random.select(ops)
    op(grammar, config)
  }
}
object MutationOr {
  def fill(ops: List[(Int, MutationOp)]): MutationOr = MutationOr(ops.flatMap { case (n, op) => List.fill(n)(op) })
}

object AddVertex extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val (label, graph) = random.select(grammar.productions)
    val vertex = Vertex(nextLabel(graph.vertices))
    val newVertices = graph.vertices + vertex
    val newVertexData = graph.vertexData ++ initVertexData().map(d => vertex -> d)
    val newGraph = graph.copy(vertices = newVertices, vertexData = newVertexData)
    Some(grammar.updateProduction(label -> newGraph))
  }
}

//TODO: this is feedforward specific
object AddConnectedVertex extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val (label, graph) = random.select(grammar.productions)
    if (graph.isEmpty) {
      val vertex = Vertex(0)
      val newVertices = Set(vertex)
      val newVertexData: Map[Vertex, V] = Map.empty ++ initVertexData().map(d => vertex -> d)
      val newGraph = graph.copy(vertices = newVertices, vertexData = newVertexData)
      Some(grammar.updateProduction(label -> newGraph))
    }
    else {
      val existingVertex = random.select(graph.vertices)

      val newVertex = Vertex(nextLabel(graph.vertices))
      val newVertices = graph.vertices + newVertex
      val newVertexData = graph.vertexData ++ initVertexData().map(d => newVertex -> d)

      val newEdge = if (random.r.nextBoolean) Edge(newVertex, existingVertex) else Edge(existingVertex, newVertex)
      // val newEdge = Edge(newVertex, existingVertex)
      val newEdges = graph.edges + newEdge
      val newEdgeData = graph.edgeData ++ initEdgeData().map(d => newEdge -> d)

      val newGraph = graph.copy(vertices = newVertices, edges = newEdges, vertexData = newVertexData, edgeData = newEdgeData)
      val result = grammar.updateProduction(label -> newGraph)
      if (feedForwardInputs.exists(result.expand.inDegree(_) > 0) ||
        feedForwardOutputs.exists(result.expand.outDegree(_) > 0)) None else Some(result)
    }
  }
}

object MutateVertex extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val candidates = grammar.productions.filter(_._2.vertexData.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val (v, data) = random.select(graph.vertexData)
        grammar.updateProduction(label -> graph.copy(vertexData = graph.vertexData.updated(v, mutateVertexData(data))))
    }
  }
}

object RemoveVertex extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val candidates = grammar.productions.filter(_._2.nonConnectors.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val vertex = random.select(graph.nonConnectors)
        grammar.updateProduction(label -> (graph - vertex))
    }
  }
}

object AddAcyclicEdge extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val candidates = grammar.productions.filter { case (_, g) =>
      g.vertices.exists(v => (g.outDegree(v) < g.vertices.size - 1) && (g.vertices - v -- g.successors(v) -- g.depthFirstSearch(v, g.predecessors)).nonEmpty)
    } // TODO: filter feedForward inputs/outputs
    random.selectOpt(candidates).flatMap {
      case (label, graph) =>
        // Only choose vertices that are not fully connected (to all other nodes)
        val vertexInCandidates = graph.vertices.filter(v => (graph.outDegree(v) < graph.vertices.size - 1) && (graph.vertices - v -- graph.successors(v) -- graph.depthFirstSearch(v, graph.predecessors)).nonEmpty)
        val vertexIn = random.select(vertexInCandidates)
        val vertexOutCandidates = graph.vertices - vertexIn -- graph.successors(vertexIn) -- graph.depthFirstSearch(vertexIn, graph.predecessors)
        val vertexOut = random.select(vertexOutCandidates)
        val edge = Edge(vertexIn, vertexOut)
        val newGraph = (graph + edge, initEdgeData()) match {
          case (graph, Some(data)) => graph.copy(edgeData = graph.edgeData + (edge -> data))
          case (graph, None)       => graph
        }
        val result = grammar.updateProduction(label -> newGraph)
        // assert(!result.expand.hasCycle, s"$graph\nedge: $edge\n$grammar\nexpanded: ${grammar.expand}")
        // TODO: prevent creating cycles in the first place
        if (newGraph.hasCycle || result.expand.hasCycle ||
          feedForwardInputs.exists(result.expand.inDegree(_) > 0) ||
          feedForwardOutputs.exists(result.expand.outDegree(_) > 0)) None else Some(result)
    }
  }
}

object AddEdge extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val candidates = grammar.productions.filter { !_._2.isComplete }
    random.selectOpt(candidates).map {
      case (label, graph) =>
        // Only choose vertices that are not fully connected (to all other nodes)
        val vertexInCandidates = graph.vertices.filter(v => graph.outDegree(v) < graph.vertices.size - 1)
        val vertexIn = random.select(vertexInCandidates)
        val vertexOutCandidates = graph.vertices - vertexIn -- graph.successors(vertexIn)
        val vertexOut = random.select(vertexOutCandidates)
        val edge = Edge(vertexIn, vertexOut)
        val newGraph = (graph + edge, initEdgeData()) match {
          case (graph, Some(data)) => graph.copy(edgeData = graph.edgeData + (edge -> data))
          case (graph, None)       => graph
        }
        grammar.updateProduction(label -> newGraph)
    }
  }
}

object MutateEdge extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val candidates = grammar.productions.filter(_._2.edgeData.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val (v, data) = random.select(graph.edgeData)
        val newData = mutateEdgeData(data)
        grammar.updateProduction(label -> graph.copy(edgeData = graph.edgeData.updated(v, newData)))
    }
  }
}

object RemoveEdge extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val candidates = grammar.productions.filter(_._2.edges.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val edge = random.select(graph.edges)
        grammar.updateProduction(label -> (graph - edge))
    }
  }
}

object InlineNonTerminal extends MutationOp {
  def inline[V, E](graph: Graph[V, E], nonTerminal: NonTerminal, grammar: Grammar[V, E]): Graph[V, E] = {
    val autoId = AutoId(nextLabel(graph.vertices)) // TODO: avoid maxBy in default AutoId?
    graph.replaceOne(nonTerminal, grammar.productions(nonTerminal.label), autoId)
  }

  override def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val candidates = grammar.productions.filter(_._2.nonTerminals.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val nonTerminal = random.select(graph.nonTerminals)

        val inlined = inline(graph, nonTerminal, grammar)

        val result = grammar.updateProduction(label -> inlined)
        // assert(result.expand isIsomorphicTo grammar.expand, s"Inline should not affect expanded graph.\nbefore:$grammar\nexpanded:${grammar.expand}\nselected: [$label] -> $nonTerminal\nafter:$result\nexpanded:${result.expand}\n")
        result
    }
  }
}

object ExtractNonTerminal extends MutationOp {
  def extract[V, E](source: Graph[V, E], subV: Set[Vertex], newLabel: Label): (Graph[V, E], Graph[V, E]) = {
    assert(subV.nonEmpty && (subV subsetOf source.vertices))
    // println(s"source: $source")
    // println(s"subV: $subV")
    val connectedComponents = source.connectedComponents(v => source.allNeighbours(v))
    val isolatedVertices = connectedComponents.filter(component => (component subsetOf subV) && (component intersect source.connectors.toSet).isEmpty).flatten

    val extractedVertices = subV ++ source.allNeighbours(subV)
    val extractedEdges = source.inducedEdges(subV) ++ source.incidentEdges(subV)
    val extractedNonTerminals = (source.inducedNonTerminals(subV) ++ source.incidentNonTerminals(subV)) diff (source.inducedNonTerminals(subV) intersect source.incidentNonTerminals(subV))
    val extractedConnectors = (if (source.connectors.isEmpty && subV.size == source.vertices.size) subV else isolatedVertices.toList ++
      source.allNeighbours(subV) ++
      (source.connectors intersect subV.toSeq)).toList.distinct // order does not matter, it just needs to be the same as in newNonTerminal
    val extractedVertexData = source.vertexData.filterKeys(extractedVertices -- extractedConnectors)
    val extractedEdgeData = source.edgeData.filterKeys(extractedEdges)
    // println(s"extractedVertices: $extractedVertices (neighbours: ${source.neighbours(subV)} ++ neighboursOverNonTerminal: ${source.neighboursOverNonTerminals(subV)})")
    val extracted = Graph(extractedVertices, extractedEdges, extractedVertexData, extractedEdgeData, extractedNonTerminals, extractedConnectors)
    // println(s"connectors: ${extracted.connectors} (neighbours: ${source.neighbours(subV)} ++ neighboursOverNonTerminal: ${source.neighboursOverNonTerminals(subV)}) ++ allNeighbours: ${source.allNeighbours(subV)}")
    assert(extracted.connectors.nonEmpty, s"\nbefore: subGraph.connectors empty.\nsource: $source\nsubVertices: $subV\nnewVertices: $extractedVertices\nsubGraph: $extracted")

    val newNonTerminal = NonTerminal(newLabel, extracted.connectors)
    val newSource = source.copy(
      vertices = source.vertices -- extracted.nonConnectors,
      edges = source.edges -- extracted.edges,
      vertexData = source.vertexData -- extracted.nonConnectors,
      edgeData = source.edgeData -- extracted.edges,
      nonTerminals = (source.nonTerminals diff extracted.nonTerminals) :+ newNonTerminal
    )

    assert((newSource.vertices ++ extracted.vertices) == source.vertices)
    assert((newSource.edges ++ extracted.edges) == source.edges)
    assert(source.connectors.toSet subsetOf newSource.connectors.toSet)

    (newSource, extracted)
  }

  override def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config._
    val candidates = grammar.productions.filter(_._2.vertices.nonEmpty)
    random.selectOpt(candidates).flatMap {
      case (srcLabel, source) =>
        val subVertices = random.selectMinOne(source.vertices).toSet
        val newLabel = grammar.productions.keys.max + 1
        val (newSource, extracted) = extract(source, subVertices, newLabel)

        if (extracted.vertices.size < 2 || extracted.nonConnectors.isEmpty) None else {
          val result = grammar.addProduction(newLabel -> extracted).updateProduction(srcLabel -> newSource)
          // assert(result.expand isIsomorphicTo grammar.expand, s"Extract should not affect expanded graph.\nbefore:$grammar\nexpanded:${grammar.expand}\nsource: [$srcLabel] -> $source\nSubVertices: $subVertices\nExtract: $extracted\nafter:$result\nexpanded:${result.expand}\n")
          Some(result)
        }
    }
  }
}

// Apply rand production rule to another production rule's graph
object ReuseNonTerminal extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config.{random => rand, _}
    val candidates = grammar.productions.toList.combinations(2).flatMap{ case ab @ List(a, b) => List(ab, List(b, a)) }.filter {
      case List((_, source), (_, target)) => source.connectors.size <= target.vertices.size
    }.toList //TODO: optimize

    rand.selectOpt(candidates).flatMap {
      case List((srcLabel, source), (targetLabel, target)) =>
        //TODO: target.vertices or target.nonConnectors ?
        val connectors = rand.select(target.vertices, n = source.connectors.size).toList
        val nonTerminal = NonTerminal(srcLabel, connectors)

        //TODO: cycle detection!
        Try(grammar.updateProduction(targetLabel -> (target + nonTerminal))).toOption
    }
  }
}

object ReuseNonTerminalAcyclic extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], config: MutationOpConfig[V, E]) = {
    import config.{random => rand, _}
    val candidates = grammar.productions.toList.combinations(2).flatMap{ case ab @ List(a, b) => List(ab, List(b, a)) }.filter {
      case List((_, source), (_, target)) => source.connectors.size <= target.vertices.size
    }.toList //TODO: optimize

    rand.selectOpt(candidates).flatMap {
      case List((srcLabel, source), (targetLabel, target)) =>
        //TODO: target.vertices or target.nonConnectors ?
        val connectors = rand.select(target.vertices, n = source.connectors.size).toList
        val nonTerminal = NonTerminal(srcLabel, connectors)

        //TODO: cycle detection!
        Try(grammar.updateProduction(targetLabel -> (target + nonTerminal))).toOption.flatMap{ g =>
          if (g.dependencyGraph.hasCycle ||
            feedForwardInputs.exists(g.expand.inDegree(_) > 0) ||
            feedForwardOutputs.exists(g.expand.outDegree(_) > 0) ||
            g.expand.hasCycle) None else Some(g)
        }
    }
  }
}
