package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import aiolia.mutations._
import util.Try
import annotation.tailrec

class FeedForwardNetworkMutation(seed: Any) extends MutationConfig[Double, Double] {
  val operators = (
    AddVertex ::
    MutateVertex ::
    RemoveVertex ::

    AddAcyclicEdge ::
    MutateEdge ::
    RemoveEdge ::

    ExtractNonTerminal ::
    ReuseNonTerminal ::
    InlineNonTerminal ::
    //TODO? RemoveNonTerminal

    Nil
  )

  override val random = Random(seed)
  override def invariant(grammar: Grammar[Double, Double]): Boolean = !grammar.expand.hasCycle
  override def initVertexData() = Some(random.r.nextGaussian)
  override def initEdgeData() = Some(random.r.nextGaussian)
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian * 0.1
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian * 0.1
}

trait MutationOpConfig[V, E] {
  val random: Random
  def initVertexData(): Option[V] = None
  def initEdgeData(): Option[E] = None
  def mutateVertexData(d: V): V = d
  def mutateEdgeData(d: E): E = d
}

trait MutationConfig[V, E] extends MutationOpConfig[V, E] {
  val operators: List[MutationOp]
  def invariant(grammar: Grammar[V, E]): Boolean = true
  def invariantError: String = ""
}

object Mutation {
  def apply[V, E](
    grammar: Grammar[V, E],
    config:  MutationConfig[V, E],
    n:       Int
  ): Grammar[V, E] = mutate(grammar, config, n)

  @tailrec final def mutate[V, E](
    grammar: Grammar[V, E],
    config:  MutationConfig[V, E],
    n:       Int
  ): Grammar[V, E] = {
    import config._
    assert(n >= 0)
    assert(invariant(grammar), invariantError)

    if (n == 0) grammar.cleanup
    else {
      val operator = random.select(operators) // TODO: make sure that operators is only created once
      operator(grammar, config) match {
        case None => mutate(grammar, config, n)
        case Some(mutatedGrammar) =>
          println(s"mutation $n: ${operator.getClass.getName}")
          assert(invariant(mutatedGrammar), s"\nbefore ${operator.getClass.getName}:\n$grammar\nexpanded: ${grammar.expand}\nafter ${operator.getClass.getName}: $invariantError\n${mutatedGrammar}\nexpanded: ${mutatedGrammar.expand}")
          mutate(mutatedGrammar, config, n - 1)
      }
    }
  }
}
