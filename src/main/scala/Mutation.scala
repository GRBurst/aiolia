package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import aiolia.mutations._
import util.Try
import annotation.tailrec

class DirectedGraphMutation(seed: Any) extends MutationConfig[Nothing, Nothing] {
  val operators = (
    AddVertex ::
    RemoveVertex ::

    AddEdge ::
    RemoveEdge ::

    ExtractNonTerminal ::
    ReuseNonTerminal ::
    InlineNonTerminal ::
    //TODO? RemoveNonTerminal

    Nil
  )

  override val random = Random(seed)
}

class FeedForwardNetworkMutation(seed: Any, override val feedForwardInputs: List[Vertex], override val feedForwardOutputs: List[Vertex]) extends MutationConfig[Double, Double] {
  val operators = (
    (AddVertex, 3) ::
    (MutateVertex, 6) ::
    (RemoveVertex, 1) ::

    (AddAcyclicEdge, 20) ::
    (MutateEdge, 18) ::
    (RemoveEdge, 1) ::

    (ExtractNonTerminal, 0) ::
    (ReuseNonTerminalAcyclic, 0) ::
    (InlineNonTerminal, 0) ::
    //TODO? RemoveNonTerminal

    Nil
  ).flatMap{ case (op, n) => List.fill(n)(op) }

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

  //feed forward
  val feedForwardInputs: List[Vertex] = Nil
  val feedForwardOutputs: List[Vertex] = Nil
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
      val operator = random.select(operators)
      // println(s"mutation $n: ${operator.getClass.getName}")
      operator(grammar, config) match {
        case None => mutate(grammar, config, n)
        case Some(mutatedGrammar) =>
          assert(invariant(mutatedGrammar), s"\nbefore ${operator.getClass.getName}:\n$grammar\nexpanded: ${grammar.expand}\nafter ${operator.getClass.getName}: $invariantError\n${mutatedGrammar}\nexpanded: ${mutatedGrammar.expand}")
          mutate(mutatedGrammar, config, n - 1)
      }
    }
  }
}
