package aiolia.grammar

import aiolia.geneticAlgorithm._
import aiolia.graph._

trait FeedForwardGrammarOpConfig extends DataGraphGrammarOpConfig[Double, Double] {
  val feedForwardInputs: List[Vertex]
  val feedForwardOutputs: List[Vertex]
}

trait GeneticAlgorithmFeedForwardConfig extends Config[Grammar[Double, Double]] with FeedForwardGrammarOpConfig { config =>

  val mutationOperators = (
    2 -> AddAcyclicEdge(config) ::
    1 -> SplitEdge(config) ::
    1 -> Shrink(config) ::
    2 -> MutateVertex(config) ::
    2 -> MutateEdge(config) ::

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
