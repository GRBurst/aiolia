package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import aiolia.mutations._
import util.Try
import annotation.tailrec

object Mutation {
  // TODO: Mutate is still not perfectly deterministic!
  // This can happen when iterating over HashSets, MashMaps ...

  val directedGraphOperators: List[MutationOp] =
    AddVertex ::
      AddEdge ::
      RemoveVertex ::
      RemoveEdge ::
      InlineNonTerminal ::
      ExtractNonTerminal ::
      ReuseNonTerminal ::
      Nil

  def mutateDirected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    mutate(grammar, directedGraphOperators, random, n)
  }

  val directedAcyclicGraphOperators: List[MutationOp] =
    AddVertex ::
      AddAcyclicEdge ::
      RemoveVertex ::
      RemoveEdge ::
      InlineNonTerminal ::
      ExtractNonTerminal ::
      // ReuseNonTerminal ::
      Nil

  def mutateDirectedAcyclic[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    mutate(grammar, directedAcyclicGraphOperators, random, n,
      (g: Grammar[V, E]) => !g.expand.hasCycle)
  }

  val directedConnectedGraphOperators: List[MutationOp] =
    AddConnectedVertex ::
      AddEdge ::
      //TODO: removeConnectedVertex ::
      //TODO: removeConnectedEdge ::
      InlineNonTerminal ::
      ExtractNonTerminal ::
      // ReuseNonTerminal ::
      Nil

  def mutateDirectedConnected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    mutate(grammar, directedConnectedGraphOperators, random, n,
      (g: Grammar[V, E]) => g.expand.isConnected,
      s"not connected: ${grammar.expand}")
  }

  @tailrec private def mutate[V, E](
    grammar:        Grammar[V, E],
    operators:      List[MutationOp],
    random:         Random,
    n:              Int,
    invariant:      Grammar[V, E] => Boolean = (_: Grammar[V, E]) => true,
    invariantError: => String                = ""
  ): Grammar[V, E] = {
    assert(n >= 0)
    assert(invariant(grammar), invariantError)

    if (n == 0) grammar.cleanup
    else {
      val operator = random.select(operators)
      operator(grammar, random) match {
        case None => mutate(grammar, operators, random, n, invariant, invariantError)
        case Some(mutatedGrammar) =>
          println(s"mutation $n: ${operator.getClass.getName}")
          assert(invariant(mutatedGrammar), s"\nbefore ${operator.getClass.getName}:\n$grammar\nexpanded: ${grammar.expand}\nafter ${operator.getClass.getName}: $invariantError\n${mutatedGrammar}\nexpanded: ${mutatedGrammar.expand}")
          mutate(mutatedGrammar, operators, random, n - 1, invariant, invariantError)
      }
    }
  }
}
