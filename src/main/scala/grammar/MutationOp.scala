package aiolia.grammar

import aiolia.geneticAlgorithm._
import aiolia.graph._
import aiolia.util.AutoId

import scala.util.Try

object Helper {
  def nextLabel(it: Iterable[{ val label: Label }]) = Try(it.maxBy(_.label).label + 1).getOrElse(0)
}
import aiolia.grammar.Helper._

// TODO: Mutate is still not perfectly deterministic!
// This can happen when iterating over HashSets, MashMaps ...

case class AddVertex[V, E](config: DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val (label, graph) = random.select(grammar.productions)
    val vertex = Vertex(nextLabel(graph.vertices))
    val newVertices = graph.vertices + vertex
    val newVertexData = graph.vertexData ++ initVertexData().map(d => vertex -> d)
    val newGraph = graph.copy(vertices = newVertices, vertexData = newVertexData)
    Some(grammar.updateProduction(label -> newGraph))
  }
}

case class AddConnectedVertex(config: FeedForwardGrammarOpConfig) extends MutationOp[Grammar[Double, Double]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val (label, graph) = random.select(grammar.productions)
    if (graph.isEmpty) {
      val vertex = Vertex(0)
      val newVertices = Set(vertex)
      val newVertexData: Map[Vertex, Double] = Map.empty ++ initVertexData().map(d => vertex -> d)
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

case class MutateVertex[V, E](config: DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val candidates = grammar.productions.filter(_._2.vertexData.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val (v, data) = random.select(graph.vertexData)
        grammar.updateProduction(label -> graph.copy(vertexData = graph.vertexData.updated(v, mutateVertexData(data))))
    }
  }
}

case class RemoveVertex[V, E](config: MutationOpConfig[Grammar[V, E]]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val candidates = grammar.productions.filter(_._2.nonConnectors.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val vertex = random.select(graph.nonConnectors)
        grammar.updateProduction(label -> (graph - vertex))
    }
  }
}

case class AddAcyclicEdge(config: FeedForwardGrammarOpConfig) extends MutationOp[Grammar[Double, Double]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val candidates = grammar.productions.filter {
      case (_, g) =>
        g.vertices.exists(v => (g.outDegree(v) < g.vertices.size - 1) && (g.vertices - v -- g.successors(v) -- g.depthFirstSearch(v, g.predecessors)).nonEmpty)
    }
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
        // TODO: prevent creating cycles in the first place -> in/out connectors?
        if (newGraph.hasCycle || result.expand.hasCycle ||
          feedForwardInputs.exists(result.expand.inDegree(_) > 0) ||
          feedForwardOutputs.exists(result.expand.outDegree(_) > 0)) None else Some(result)
    }
  }
}

case class AddEdge[V, E](config: DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
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

case class MutateEdge[V, E](config: DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
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

case class RemoveEdge[V, E](config: MutationOpConfig[Grammar[V, E]]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val candidates = grammar.productions.filter(_._2.edges.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val edge = random.select(graph.edges)
        grammar.updateProduction(label -> (graph - edge))
    }
  }
}

case class InlineNonTerminal[V, E](config: DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def inline(graph: Graph[V, E], nonTerminal: NonTerminal, grammar: Grammar[V, E]): Graph[V, E] = {
    val autoId = AutoId(nextLabel(graph.vertices)) // TODO: avoid maxBy in default AutoId?
    graph.replaceOne(nonTerminal, grammar.productions(nonTerminal.label), autoId)
  }

  def apply(grammar: Genotype): Option[Genotype] = {
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

object ExtractNonTerminal {
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
}

case class ExtractNonTerminal[V, E](config: DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val candidates = grammar.productions.filter(_._2.vertices.nonEmpty)
    random.selectOpt(candidates).flatMap {
      case (srcLabel, source) =>
        val subVertices = random.selectMinOne(source.vertices).toSet
        val newLabel = grammar.productions.keys.max + 1
        val (newSource, extracted) = ExtractNonTerminal.extract(source, subVertices, newLabel)

        if (extracted.vertices.size < 2 || extracted.nonConnectors.isEmpty) None else {
          val result = grammar.addProduction(newLabel -> extracted).updateProduction(srcLabel -> newSource)
          // assert(result.expand isIsomorphicTo grammar.expand, s"Extract should not affect expanded graph.\nbefore:$grammar\nexpanded:${grammar.expand}\nsource: [$srcLabel] -> $source\nSubVertices: $subVertices\nExtract: $extracted\nafter:$result\nexpanded:${result.expand}\n")
          Some(result)
        }
    }
  }
}

// Apply rand production rule to another production rule's graph
case class ReuseNonTerminal[V, E](config: DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config.{random => rand}
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

case class ReuseNonTerminalAcyclic(config: FeedForwardGrammarOpConfig) extends MutationOp[Grammar[Double, Double]] {
  def apply(grammar: Genotype): Option[Genotype] = {
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
