package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import aiolia.mutations._
import util.Try
import annotation.tailrec

class BugHunter(seed: Any) extends MutationConfig[Double, String] {
  val operators = (
    AddVertex ::
    RemoveVertex ::
    AddEdge ::
    RemoveEdge ::
    ExtractNonTerminal ::
    InlineNonTerminal ::
    Nil
  )

  override val random: Random = Random(seed)
  override def initVertexData() = Some(random.r.nextGaussian)
  override def initEdgeData() = Some(random.r.nextGaussian.toString)
  override def mutateVertexData(d: Double) = d + 1
  override def mutateEdgeData(d: String) = d + "a"
}

class DirectedGraphMutation(seed: Any) extends MutationConfig[Nothing, Nothing] {
  val operators = (
    AddVertex ::
    // RemoveVertex ::

    AddEdge ::
    // RemoveEdge ::

    ExtractNonTerminal ::
    // ReuseNonTerminal ::
    InlineNonTerminal ::
    //TODO? RemoveNonTerminal

    Nil
  )

  override val random = Random(seed)
}

class FeedForwardNetworkMutation(seed: Any, override val feedForwardInputs: List[Vertex], override val feedForwardOutputs: List[Vertex]) extends MutationConfig[Double, Double] {
  val operators = (
    1 -> AddConnectedVertex ::
    1 -> MutateVertex ::
    1 -> RemoveVertex ::

    3 -> AddAcyclicEdge ::
    1 -> MutateEdge ::
    1 -> RemoveEdge ::

    0 -> ExtractNonTerminal ::
    0 -> ReuseNonTerminalAcyclic ::
    0 -> InlineNonTerminal ::
    //TODO? RemoveNonTerminal

    Nil
  ).flatMap{ case (n, op) => List.fill(n)(op) }

  override val random = Random(seed)
  override def invariant(grammar: Grammar[Double, Double]): Boolean = (
    !grammar.expand.hasCycle &&
    feedForwardInputs.forall(grammar.expand.inDegree(_) == 0) &&
    feedForwardOutputs.forall(grammar.expand.outDegree(_) == 0)
  )
  override def initVertexData() = Some(random.r.nextGaussian)
  override def initEdgeData() = Some(random.r.nextGaussian)
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian * 0.01
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian * 0.01
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
  // TODO: post processing of grammar, eg: clean up isolated vertices
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
      val tries = new Iterator[Option[Grammar[V, E]]] {
        def hasNext = true
        def next = {
          // println(s"  mutation $n: ${operator.getClass.getName}")
          operator(grammar, config)
        }
      }

      // println("start trying...")
      val maxTries = 5
      val result = tries.take(maxTries).flatten.take(1).toList
      // println("done")

      result match {
        case Nil => mutate(grammar, config, n)
        case List(mutatedGrammar) =>
          assert(invariant(mutatedGrammar), s"\nbefore ${operator.getClass.getName}:\n$grammar\nexpanded: ${grammar.expand}\nafter ${operator.getClass.getName}: $invariantError\n${mutatedGrammar}\nexpanded: ${mutatedGrammar.expand}")
          mutate(mutatedGrammar, config, n - 1)
      }
    }
  }
}
