package aiolia.mutations

import aiolia.Grammar
import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import util.Try

trait MutationOp {
  def nextLabel(it: Iterable[{ val label: Label }]) = Try(it.maxBy(_.label).label + 1).getOrElse(0)

  def apply[V, E](grammar: Grammar[V, E], rand: Random): Option[Grammar[V, E]]
}

object AddVertex extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], rand: Random) = {
    val (label, graph) = rand.select(grammar.productions)
    val vertex = Vertex(nextLabel(graph.vertices))
    Some(grammar.updateProduction(label -> (graph + vertex)))
  }
}

object AddConnectedVertex extends MutationOp {
  def apply[V, E](grammar: Grammar[V, E], rand: Random) = {
    val candidates = grammar.productions
    rand.selectOpt(candidates).map {
      case (label, graph) =>
        if (graph.isEmpty)
          grammar.updateProduction(label -> (graph + Vertex(0)))
        else {
          val existingVertex = rand.select(graph.vertices)
          val newVertex = Vertex(nextLabel(graph.vertices))
          val newEdge = if (rand.r.nextBoolean) Edge(newVertex, existingVertex) else Edge(existingVertex, newVertex)
          val result = grammar.updateProduction(label -> (graph + newVertex + newEdge))
          assert(result.expand.isConnected)
          result
        }
    }
  }
}

object AddEdge extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], rand: Random) = {
    val candidates = grammar.productions.filter { !_._2.isComplete }
    rand.selectOpt(candidates).map {
      case (label, graph) =>
        // Only choose vertices that are not fully connected (to all other nodes)
        val vertexInCandidates = graph.vertices.filter(v => graph.outDegree(v) < graph.vertices.size - 1)
        val vertexIn = rand.select(vertexInCandidates)
        val vertexOutCandidates = graph.vertices - vertexIn -- graph.successors(vertexIn)
        val vertexOut = rand.select(vertexOutCandidates)
        val edge = Edge(vertexIn, vertexOut)
        grammar.updateProduction(label -> (graph + edge))
    }
  }
}

object RemoveVertex extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], rand: Random) = {
    val candidates = grammar.productions.filter(_._2.nonConnectors.nonEmpty)
    rand.selectOpt(candidates).map {
      case (label, graph) =>
        val vertex = rand.select(graph.nonConnectors)
        grammar.updateProduction(label -> (graph - vertex))
    }
  }
}

object RemoveEdge extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], rand: Random) = {
    val candidates = grammar.productions.filter(_._2.edges.nonEmpty)
    rand.selectOpt(candidates).map {
      case (label, graph) =>
        val edge = rand.select(graph.edges)
        grammar.updateProduction(label -> (graph - edge))
    }
  }
}

//TODO? def removeNonTerminal[V,E](grammar:Grammar[V,E], rand:Random):Option[Grammar[V,E]]

object InlineNonTerminal extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], rand: Random) = {
    val candidates = grammar.productions.filter(_._2.nonTerminals.nonEmpty)
    rand.selectOpt(candidates).map {
      case (label, graph) =>
        val nonTerminal = rand.select(graph.nonTerminals)
        val autoId = new AutoId(start = nextLabel(graph.vertices)) // TODO: avoid maxBy in default AutoId?
        val inlined = graph.replaceOne(nonTerminal, grammar.productions(nonTerminal.label), autoId)

        val result = grammar.updateProduction(label -> inlined)
        assert(result.expand == grammar.expand, s"Inline should not affect expanded graph.\nbefore:$grammar\nafter:$result")
        result
    }
  }
}

object ExtractNonTerminal extends MutationOp {
  def extract[V, E](source: Graph[V, E], subV: Set[Vertex], newLabel: Label): (Graph[V, E], Graph[V, E]) = {
    assert(subV.nonEmpty && (subV subsetOf source.vertices))
    val newVertices = subV ++ source.neighbours(subV) ++ source.neighboursOverNonTerminals(subV)
    val extracted = Graph(
      vertices = newVertices,
      edges = source.inducedEdges(subV) ++ source.incidentEdges(subV),
      nonTerminals = source.inducedNonTerminals(subV) ++ source.incidentNonTerminals(subV),
      connectors = (subV.filter(source.degree(_) == 0).toList ++ source.neighbours(subV) ++ source.neighboursOverNonTerminals(subV) ++ (source.connectors intersect subV.toSeq)).toList.distinct // order does not matter, it just needs to be the same as in newNonTerminal
    )
    assert(extracted.connectors.nonEmpty, s"\nbefore: subGraph.connectors empty.\nsource: $source\nsubVertices: $subV\nnewVertices: $newVertices\nsubGraph: $extracted")

    val newNonTerminal = NonTerminal(newLabel, extracted.connectors)
    val newSource = source.copy(
      vertices = source.vertices -- extracted.nonConnectors,
      edges = source.edges -- extracted.edges,
      nonTerminals = (source.nonTerminals diff extracted.nonTerminals) :+ newNonTerminal
    )

    (newSource, extracted)
  }

  override def apply[V, E](grammar: Grammar[V, E], rand: Random) = {
    val candidates = grammar.productions.filter(_._2.vertices.nonEmpty)
    rand.selectOpt(candidates).map {
      case (srcLabel, source) =>
        val subVertices = rand.selectMinOne(source.vertices).toSet
        val newLabel = grammar.productions.keys.max + 1
        val (newSource, extracted) = extract(source, subVertices, newLabel)

        val result = grammar.addProduction(newLabel -> extracted).updateProduction(srcLabel -> newSource)
        assert(result.expand == grammar.expand, s"Extract should not affect expanded graph.\nbefore:$grammar\nexpanded:${grammar.uniqueVertices.expand}\nsource: [$srcLabel] -> $source\nExtract: $extracted\nafter:$result\nexpanded:${result.uniqueVertices.expand}\n")
        result
    }
  }
}

// Apply rand production rule to another production rule's graph
object ReuseNonTerminal extends MutationOp {
  override def apply[V, E](grammar: Grammar[V, E], rand: Random) = {
    val candidates = grammar.productions.toList.combinations(2).filter {
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
