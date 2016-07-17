package aiolia.grammar

import aiolia.graph._
import aiolia.util.AutoId

import scala.util.Try

object Helper {
  // def nextLabel(it: Iterable[{ val label: Label }]) = Try(it.maxBy(_.label).label + 1).getOrElse(0) // does not work when Vertex is a value class
  def nextLabel(it: Iterable[Vertex]) = Try(it.maxBy(_.label).label + 1).getOrElse(0)
}
import aiolia.grammar.Helper._

trait MutationOp[G] extends ((G) => Option[G]) {
  type Genotype = G
}

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

case class SplitEdge(config: InOutGrammarOpConfig[Double, Double]) extends MutationOp[Grammar[Double, Double]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val requirement = (graph: Graph[Double, Double], edge: Edge) => edge match { case Edge(in, out) => (graph.inDegree(in) > 1 || graph.connectors.contains(in)) && (graph.outDegree(out) > 1 || graph.connectors.contains(out)) }
    val candidates = grammar.productions.filter { case (_, graph) => graph.edges.exists(requirement(graph, _)) }
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val edge = random.select(graph.edges.filter(requirement(graph, _)))
        // (In) -edge-> (Out)
        // (In) -e1-> (n) -e2-> (Out)
        val middleNode = Vertex(nextLabel(graph.vertices))
        val e1 = Edge(edge.in, middleNode)
        val e2 = Edge(middleNode, edge.out)
        val newVertexData: Map[Vertex, Double] = graph.vertexData + (middleNode -> 0.0)
        val newEdgeData: Map[Edge, Double] = graph.edgeData + (e1 -> 1.0) + (e2 -> graph.edgeData(edge)) - edge
        val newGraph = graph.copy(
          vertices = graph.vertices + middleNode,
          edges = graph.edges + e1 + e2 - edge,
          vertexData = newVertexData,
          edgeData = newEdgeData
        )
        grammar.updateProduction(label -> newGraph)
    }
  }
}

case class ReconnectEdge[V, E](config: InOutGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    def requirement(graph: Graph[_, _], edge: Edge) = edge match { case Edge(in, out) => (graph.inDegree(in) > 1 || graph.connectors.contains(in)) && (graph.outDegree(out) > 1 || graph.connectors.contains(out)) }
    val candidates = grammar.productions.filter { case (_, graph) => graph.edges.exists(requirement(graph, _)) }
    random.selectOpt(candidates).flatMap {
      case (label, graph) =>
        import graph._
        val edge = random.select(edges.filter(requirement(graph, _)))
        def newOut = random.select(vertices -- depthFirstSearch(edge.in, predecessors)) //TODO: selectOpt
        def newIn = random.select(vertices -- depthFirstSearch(edge.in, successors)) //TODO: selectOpt -- already crashed once!
        val newEdge = if (random.r.nextBoolean) Edge(edge.in, newOut) else Edge(newIn, edge.out)
        val newGraph = graph.copy(
          vertices = vertices,
          edges = edges - edge + newEdge,
          edgeData = edgeData - edge + (newEdge -> edgeData(edge))
        )
        //TODO: choose non-violating vertices in the first place
        val result = grammar.updateProduction(label -> newGraph)
        if (newGraph.hasCycle || result.expand.hasCycle ||
          inputs.exists(result.expand.inDegree(_) > 0) ||
          outputs.exists(result.expand.outDegree(_) > 0) ||
          (result.expand.vertices -- inputs -- outputs).exists(v => result.expand.inDegree(v) == 0 || result.expand.outDegree(v) == 0)) None else Some(result)
    }
  }
}

case class Shrink(config: InOutGrammarOpConfig[Double, Double]) extends MutationOp[Grammar[Double, Double]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val candidates = grammar.productions.filter(_._2.nonConnectors.nonEmpty)
    random.selectOpt(candidates).map {
      case (label, graph) =>
        import graph._
        val vertex = random.select(nonConnectors)
        // (In) -e1-> (n) -e2-> (Out)
        // (In) -edge-> (Out)
        val removedEdges = incidentEdges(vertex)
        val incoming = incomingEdges(vertex)
        val outgoing = outgoingEdges(vertex)
        // val edgeCount = incoming.size max outgoing.size
        val edgeCount = removedEdges.size
        val ins = random.r.shuffle(Stream.continually(incoming).flatten.take(edgeCount))
        val outs = random.r.shuffle(Stream.continually(outgoing).flatten.take(edgeCount))
        val newEdgeData = (ins zip outs).map { case (in: Edge, out: Edge) => Edge(in.in, out.out) -> (edgeData(in) + edgeData(out)) / 2 }.toMap
        val newEdges = newEdgeData.keys

        val newGraph: Graph[Double, Double] = graph.copy(
          vertices = vertices - vertex,
          edges = edges ++ newEdges -- removedEdges,
          vertexData = vertexData - vertex,
          edgeData = edgeData -- removedEdges ++ newEdgeData
        )
        grammar.updateProduction(label -> newGraph)
    }
  }
}

case class AddConnectedVertex[V, E](config: DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val (label, graph) = random.select(grammar.productions)
    if (graph.isEmpty) {
      val vertex = Vertex(0)
      val newVertices = Set(vertex)
      val newVertexData = Map.empty ++ initVertexData().map(d => vertex -> d)
      val newGraph = graph.copy(vertices = newVertices, vertexData = newVertexData)
      Some(grammar.updateProduction(label -> newGraph))
    } else {
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
      // if (inputs.exists(result.expand.inDegree(_) > 0) ||
      //   outputs.exists(result.expand.outDegree(_) > 0)) None else Some(result)
      Some(result)
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

case class AddAcyclicEdge[V, E](config: InOutGrammarOpConfig[V, E] with DataGraphGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val candidates = grammar.productions.filter {
      case (_, g) =>
        g.vertices.exists(v => (g.vertices - v -- g.successors(v) -- g.depthFirstSearch(v, g.predecessors)).nonEmpty)
    }
    random.selectOpt(candidates).flatMap {
      case (label, graph) =>
        // Only choose vertices that are not fully connected (to all other nodes)
        val vertexInCandidates = graph.vertices.filter(v => (graph.vertices - v -- graph.successors(v) -- graph.depthFirstSearch(v, graph.predecessors)).nonEmpty)
        val vertexIn = random.select(vertexInCandidates)
        val vertexOutCandidates = graph.vertices - vertexIn -- graph.successors(vertexIn) -- graph.depthFirstSearch(vertexIn, graph.predecessors)
        val vertexOut = random.select(vertexOutCandidates)
        val edge = Edge(vertexIn, vertexOut)
        val newGraph = (graph + edge, initEdgeData()) match {
          case (graph, Some(data)) => graph.copy(edgeData = graph.edgeData + (edge -> data))
          case (graph, None) => graph
        }
        val result = grammar.updateProduction(label -> newGraph)
        // assert(!result.expand.hasCycle, s"$graph\nedge: $edge\n$grammar\nexpanded: ${grammar.expand}")
        // TODO: prevent creating cycles in the first place -> in/out connectors?
        if (newGraph.hasCycle || result.expand.hasCycle ||
          inputs.exists(result.expand.inDegree(_) > 0) ||
          outputs.exists(result.expand.outDegree(_) > 0)) None else Some(result)
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
          case (graph, None) => graph
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

case class RemoveInterconnectedEdge[V, E](config: MutationOpConfig[Grammar[V, E]]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config._
    val candidates = grammar.productions.filter { case (_, graph) => graph.edges.exists { case Edge(in, out) => graph.outDegree(in) > 1 && graph.inDegree(out) > 1 } }
    random.selectOpt(candidates).map {
      case (label, graph) =>
        val edge = random.select(graph.edges.filter { case Edge(in, out) => graph.outDegree(in) > 1 && graph.inDegree(out) > 1 })
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
    val candidates = grammar.productions.toList.combinations(2).flatMap { case ab @ List(a, b) => List(ab, List(b, a)) }.filter {
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

case class ReuseNonTerminalAcyclic[V, E](config: InOutGrammarOpConfig[V, E]) extends MutationOp[Grammar[V, E]] {
  def apply(grammar: Genotype): Option[Genotype] = {
    import config.{random => rand, _}
    val candidates = grammar.productions.toList.combinations(2).flatMap { case ab @ List(a, b) => List(ab, List(b, a)) }.filter {
      case List((_, source), (_, target)) => source.connectors.size <= target.vertices.size
    }.toList //TODO: optimize

    rand.selectOpt(candidates).flatMap {
      case List((srcLabel, source), (targetLabel, target)) =>
        //TODO: target.vertices or target.nonConnectors ?
        val connectors = rand.select(target.vertices, n = source.connectors.size).toList
        val nonTerminal = NonTerminal(srcLabel, connectors)

        //TODO: cycle detection!
        Try(grammar.updateProduction(targetLabel -> (target + nonTerminal))).toOption.flatMap { g =>
          if (g.dependencyGraph.hasCycle ||
            inputs.exists(g.expand.inDegree(_) > 0) ||
            outputs.exists(g.expand.outDegree(_) > 0) ||
            g.expand.hasCycle) None else Some(g)
        }
    }
  }
}

case class SplitWireAddGate(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
  def apply(graph: Genotype): Option[Genotype] = {
    import graph._
    import config._
    random.selectOpt(graph.edges).map {
      case replacedEdge @ Edge(in, out) =>
        val gate = Vertex(nextLabel(vertices))
        val ein1 = Edge(in, gate)
        val eout = Edge(gate, out)
        val ein2 = if (random.r.nextBoolean) {
          random.selectOpt(vertices -- outputs -- depthFirstSearch(in, successors)).map { in => Edge(in, gate) }
        } else None

        val newGraph = graph.copy(
          vertices = vertices + gate,
          edges = edges - replacedEdge + ein1 + eout ++ ein2
        )
        newGraph
    }
  }
}

case class OutputXor(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
  def apply(graph: Genotype): Option[Genotype] = {
    import graph._
    import config._
    random.selectOpt(outputs.filter(inDegree(_) > 0)).map { output =>
      val replacedEdge: Edge = random.select(incomingEdges(output))
      val signalA = replacedEdge.in
      val signalB = random.select(vertices -- outputs - signalA -- predecessors(output))

      val newGraph = Gates.insertXOR(signalA, signalB, output, graph.copy(
        edges = edges - replacedEdge
      ))
      newGraph
    }
  }
}

case class InnerXor(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
  def apply(graph: Genotype): Option[Genotype] = {
    import graph._
    import config._
    random.selectOpt((vertices -- inputs -- outputs).filter(inDegree(_) > 0)).flatMap { output =>
      val replacedEdge: Edge = random.select(incomingEdges(output))
      val signalA = replacedEdge.in
      random.selectOpt(vertices -- outputs - signalA -- predecessors(output) -- depthFirstSearch(signalA, successors)) map { signalB =>
        val newGraph = Gates.insertXOR(signalA, signalB, output, graph.copy(
          edges = edges - replacedEdge
        ))
        newGraph
      }
    }
  }
}

object Gates {
  def insert1MUX(signalA: Vertex, switch: Vertex, signalB: Vertex, output: Vertex, graph: Graph[Nothing, Nothing]): Graph[Nothing, Nothing] = {
    import graph._
    val autoId = AutoId(nextLabel(vertices))

    // 1-MUX (signalA, switch, signalB) --> output
    val gateA = Vertex(autoId.nextId)
    val gateB = Vertex(autoId.nextId)
    val gateC = Vertex(autoId.nextId)
    val gateD = Vertex(autoId.nextId)
    val muxGates = gateA :: gateB :: gateC :: gateD :: Nil
    val muxWires = List(
      Edge(switch, gateA),

      Edge(signalA, gateB),
      Edge(gateA, gateB),
      Edge(switch, gateC),
      Edge(signalB, gateC),

      Edge(gateB, gateD),
      Edge(gateC, gateD),

      Edge(gateD, output)
    )

    graph.copy(
      vertices = vertices ++ muxGates,
      edges = edges ++ muxWires
    )
  }

  def insertXOR(signalA: Vertex, signalB: Vertex, output: Vertex, graph: Graph[Nothing, Nothing]): Graph[Nothing, Nothing] = {
    import graph._
    val autoId = AutoId(nextLabel(vertices))

    // XOR (signalA,signalB) --> output
    val gateA = Vertex(autoId.nextId)
    val gateB = Vertex(autoId.nextId)
    val gateC = Vertex(autoId.nextId)
    val gateD = Vertex(autoId.nextId)
    val xorGates = gateA :: gateB :: gateC :: gateD :: Nil
    val xorWires = List(
      Edge(signalA, gateA),
      Edge(signalA, gateB),
      Edge(signalB, gateA),
      Edge(signalB, gateC),

      Edge(gateA, gateB),
      Edge(gateA, gateC),

      Edge(gateB, gateD),
      Edge(gateC, gateD),

      Edge(gateD, output)
    )

    graph.copy(
      vertices = vertices ++ xorGates,
      edges = edges ++ xorWires
    )
  }
}

case class ReplaceInnerWireBy1Mux(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
  def apply(graph: Genotype): Option[Genotype] = {
    import graph._
    import config._
    val outputCandidates = (vertices -- inputs -- outputs).filter(inDegree(_) > 0)
    random.selectOpt(outputCandidates).flatMap { output =>
      val replacedEdge: Edge = random.select(incomingEdges(output))
      val signalA = replacedEdge.in
      val additionalInputCandidates = vertices -- outputs - signalA -- predecessors(output) -- depthFirstSearch(signalA, successors)
      random.selectOpt(additionalInputCandidates, 2).map(_.toSeq) map {
        case Seq(signalB, switch) =>
          val newGraph = Gates.insert1MUX(signalA, switch, signalB, output, graph.copy(
            edges = edges - replacedEdge
          ))
          newGraph
      }
    }
  }
}

case class ReplaceOutputWireBy1Mux(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
  def apply(graph: Genotype): Option[Genotype] = {
    import graph._
    import config._
    val outputCandidates = (outputs).filter(inDegree(_) > 0)
    random.selectOpt(outputCandidates).flatMap { output =>
      val replacedEdge: Edge = random.select(incomingEdges(output))
      val signalA = replacedEdge.in
      val additionalInputCandidates = vertices -- outputs - signalA -- predecessors(output) -- depthFirstSearch(signalA, successors)
      random.selectOpt(additionalInputCandidates, 2).map(_.toSeq) map {
        case Seq(signalB, switch) =>
          val newGraph = Gates.insert1MUX(signalA, switch, signalB, output, graph.copy(
            edges = edges - replacedEdge
          ))
          newGraph
      }
    }
  }
}

case class AddAcyclicWireOrInverter(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
  def apply(graph: Genotype): Option[Genotype] = {
    import graph._
    import config._

    val nonInputs = vertices -- inputs
    def outCandidates(v: Vertex) = (nonInputs -- successors(v) -- depthFirstSearch(v, predecessors))

    // Only choose vertices that are not fully forwards connected (to all other successor nodes)
    val vertexInCandidates = (vertices -- outputs).map(v => (v -> outCandidates(v))).filter(_._2.nonEmpty)

    random.selectOpt(vertexInCandidates).map {
      case (vertexIn, vertexOutCandidates) =>
        val vertexOut = random.select(vertexOutCandidates)
        val newGraph = if (random.r.nextBoolean) {
          // just a wire
          val wire = Edge(vertexIn, vertexOut)
          graph + wire
        } else {
          // add inverter edge
          val inverter = Vertex(nextLabel(vertices))
          val wireIn = Edge(vertexIn, inverter)
          val wireOut = Edge(inverter, vertexOut)
          graph + inverter + wireIn + wireOut
        }
        assert(!newGraph.hasCycle)
        newGraph
    }
  }
}

case class RemoveWire(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
  def apply(graph: Genotype): Option[Genotype] = {
    import graph._
    import config._

    random.selectOpt(edges).map { edge =>
      graph.copy(edges = edges - edge)
    }
  }
}

case class CleanUpCircuit(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
  def apply(graph: Genotype): Option[Genotype] = {
    import graph._
    import config._
    //TODO: circuit simplification

    val keepVertices = (outputs ++ inputs ++ outputs.flatMap(depthFirstSearch(_, predecessors))).toSet
    val keepInnerVertices = keepVertices -- outputs -- inputs
    val removeVertices = vertices -- keepVertices
    val autoId = AutoId(nextLabel(inputs ++ outputs))
    val vertexMap = (keepInnerVertices.map(_.label) zip autoId).toMap.withDefault((k: Label) => k)
    val newGraph = (graph -- removeVertices) mapVertices vertexMap
    Some(newGraph)
  }
}

// case class MergeWiresRemoveGate(config: CircuitConfig) extends MutationOp[Graph[Nothing, Nothing]] {
//   def apply(graph: Genotype): Option[Genotype] = {
//     import graph._
//     import config._
//     random.selectOpt(graph.vertices -- inputs -- outputs).map {
//       case removedGate =>
//         val gate = Vertex(nextLabel(vertices))
//         val ein1 = Edge(edge.in, gate)
//         val eout = Edge(gate, edge.out)
//         val additionalInput = random.select(vertices -- outputs -- depthFirstSearch(in, predecessors))
//         val ein2 = Edge(additionalInput, gate)

//         val newGraph = copy(
//           vertices = vertices + gate,
//           edges = edges - replacedEdge + ein1 + ein2 + eout
//         )
//         newGraph
//     }
//   }
// }
