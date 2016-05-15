package aiolia.grammar

import aiolia.geneticAlgorithm._
import aiolia.graph._

trait FeedForwardGrammarOpConfig extends DataGraphGrammarOpConfig[Double, Double] {
  val feedForwardInputs: List[Vertex]
  val feedForwardOutputs: List[Vertex]
}

trait GeneticAlgorithmFeedForwardConfig extends Config[Grammar[Double, Double]] with FeedForwardGrammarOpConfig { config =>

  val mutationOperators = (
    1 -> AddAcyclicEdge(config) ::
    1 -> RemoveInterconnectedEdge(config) ::
    1 -> SplitEdge(config) ::
    1 -> ReconnectEdge(config) ::
    1 -> Shrink(config) ::
    1 -> MutateVertex(config) ::
    1 -> MutateEdge(config) ::

    Nil
  ).flatMap{ case (n, op) => List.fill(n)(op) }

  override def afterMutationOp(g: Grammar[Double, Double]) = g.cleanup

  override def initVertexData() = Some(random.r.nextDouble * 2 - 1)
  override def initEdgeData() = Some(random.r.nextDouble * 2 - 1)
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian * 0.05
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian * 0.05
  override def genotypeInvariant(grammar: Grammar[Double, Double]): Boolean = !grammar.expand.hasCycle &&
    feedForwardInputs.forall(grammar.expand.inDegree(_) == 0) &&
    feedForwardOutputs.forall(grammar.expand.outDegree(_) == 0) &&
    (grammar.expand.vertices -- feedForwardInputs -- feedForwardOutputs).forall(v => grammar.expand.inDegree(v) > 0 && grammar.expand.outDegree(v) > 0)
}
