package aiolia.grammar

import aiolia.graph._
import aiolia.util.Random

trait MutationOpConfig[G] {
  val random: Random
  def genotypeInvariant(g: G): Boolean = true
  def genotypeInvariantError(g: G): String = "Genotype invariant violated."
}

trait DataGraphGrammarOpConfig[V, E] extends MutationOpConfig[Grammar[V, E]] {
  //TODO: why default implementations?
  def initVertexData(): Option[V] = None
  def initEdgeData(): Option[E] = None
  def mutateVertexData(d: V): V = d
  def mutateEdgeData(d: E): E = d
}

trait InOutGrammarOpConfig[V, E] extends MutationOpConfig[Grammar[V, E]] {
  val inputs: List[Vertex]
  val outputs: List[Vertex]
}

trait NeuralNetworkGrammarOpConfig extends DataGraphGrammarOpConfig[Double, Double] with InOutGrammarOpConfig[Double, Double] {
  def neuronMutationStrength: Double
  def synapseMutationStrength: Double

  override def initVertexData() = Some(random.r.nextDouble * 2 - 1) // [-1, 1]
  override def initEdgeData() = Some(random.r.nextDouble * 2 - 1) // [-1, 1]
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian * neuronMutationStrength
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian * synapseMutationStrength
}

trait FeedForwardNetworkConfig extends NeuralNetworkGrammarOpConfig { config =>
  def addAcyclicEdgeFreq = 1
  def removeInterconnectedEdgeFreq = 1
  def splitEdgeFreq = 1
  def reconnectEdgeFreq = 1
  def shrinkFreq = 1
  def mutateVertexFreq = 1
  def mutateEdgeFreq = 1

  val mutationOperators = (
    addAcyclicEdgeFreq -> AddAcyclicEdge(config) ::
    removeInterconnectedEdgeFreq -> RemoveInterconnectedEdge(config) ::
    splitEdgeFreq -> SplitEdge(config) ::
    reconnectEdgeFreq -> ReconnectEdge(config) ::
    shrinkFreq -> Shrink(config) ::
    mutateVertexFreq -> MutateVertex(config) ::
    mutateEdgeFreq -> MutateEdge(config) ::
    // 1 -> ExtractNonTerminal(config) ::
    // 1 -> ReuseNonTerminalAcyclic(config) ::
    // 1 -> InlineNonTerminal(config) ::
    Nil
  ).flatMap { case (n, op) => List.fill(n)(op) }

  // override def afterMutationOp(g: Grammar[Double, Double]) = g.cleanup
  override def genotypeInvariant(grammar: Grammar[Double, Double]): Boolean = !grammar.expand.hasCycle &&
    inputs.forall(grammar.expand.inDegree(_) == 0) &&
    outputs.forall(grammar.expand.outDegree(_) == 0) &&
    (grammar.expand.vertices -- inputs -- outputs).forall(v => grammar.expand.inDegree(v) > 0 && grammar.expand.outDegree(v) > 0)
}

trait CircuitConfig extends MutationOpConfig[Graph[Nothing, Nothing]] { config =>
  val inputs: List[Vertex]
  val outputs: List[Vertex]

  val mutationOperators = (
    20 -> OutputXor(config) ::
    30 -> InnerXor(config) ::
    10 -> AddAcyclicWireOrInverter(config) ::
    10 -> SplitWireAddGate(config) ::
    10 -> RemoveWire(config) ::
    1 -> CleanUpCircuit(config) ::
    // 1 -> MergeWiresRemoveGate(config) ::
    Nil
  ).flatMap { case (n, op) => List.fill(n)(op) }

  override def genotypeInvariant(graph: Graph[Nothing, Nothing]): Boolean = !graph.hasCycle &&
    inputs.forall(graph.inDegree(_) == 0) &&
    outputs.forall(graph.outDegree(_) == 0) &&
    (graph.vertices -- inputs).forall(v => (graph.inDegree(v) <= 2))
  override def genotypeInvariantError(graph: Graph[Nothing, Nothing]): String = s"""Genotype Invariant violated
  inDegree != 0:  ${inputs.filterNot(graph.inDegree(_) == 0)}
  outDegree != 0: ${outputs.filterNot(graph.outDegree(_) == 0)}
  inDegree > 2:   ${(graph.vertices -- inputs).filterNot(v => (graph.inDegree(v) <= 2))}"""
}
