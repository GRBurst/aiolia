package aiolia.grammar

import aiolia.geneticAlgorithm._
import aiolia.graph._

trait DataGraphGrammarOpConfig[V, E] extends MutationOpConfig[Grammar[V, E]] {
  def initVertexData(): Option[V] = None
  def initEdgeData(): Option[E] = None
  def mutateVertexData(d: V): V = d
  def mutateEdgeData(d: E): E = d
}

trait FeedForwardGrammarOpConfig extends DataGraphGrammarOpConfig[Double, Double] {
  val feedForwardInputs: List[Vertex]
  val feedForwardOutputs: List[Vertex]
}

trait GeneticAlgorithmFeedForwardConfig extends Config[Grammar[Double, Double]] with FeedForwardGrammarOpConfig { config =>

  val mutationOperators = (
    1 -> AddConnectedVertex(config) ::
    1 -> MutateVertex(config) ::
    1 -> RemoveVertex(config) ::

    3 -> AddAcyclicEdge(config) ::
    1 -> MutateEdge(config) ::
    1 -> RemoveEdge(config) ::

    0 -> ExtractNonTerminal(config) ::
    0 -> ReuseNonTerminalAcyclic(config) ::
    0 -> InlineNonTerminal(config) ::
    //TODO? RemoveNonTerminal

    Nil
  ).flatMap{ case (n, op) => List.fill(n)(op) }

  override def afterMutationOp(g: Grammar[Double, Double]) = g.cleanup

  override def initVertexData() = Some(random.r.nextGaussian)
  override def initEdgeData() = Some(random.r.nextGaussian)
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian * 0.01
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian * 0.01
  override def genotypeInvariant(grammar: Grammar[Double, Double]): Boolean = !grammar.expand.hasCycle &&
    feedForwardInputs.forall(grammar.expand.inDegree(_) == 0) &&
    feedForwardOutputs.forall(grammar.expand.outDegree(_) == 0)
}

