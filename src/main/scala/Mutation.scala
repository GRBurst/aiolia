package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers.{Random, AutoId}
import aiolia.mutations._
import util.Try

object Mutation {
  // TODO: Mutate is still not perfectly deterministic!
  // This can happen when iterating over HashSets, MashMaps ...

  val directedGraphOperators: List[MutationOp] = List(
    AddVertex,
    AddEdge,
    RemoveVertex,
    RemoveEdge,
    InlineNonTerminal,
    ExtractNonTerminal,
    ReuseNonTerminal
  )

  val directedConnectedGraphOperators: List[MutationOp] = List(
    AddConnectedVertex,
    AddEdge,
    //TODO: removeConnectedVertex,
    //TODO: removeConnectedEdge,
    // InlineNonTerminal,
    ExtractNonTerminal
  // ReuseNonTerminal
  )

  val directedAcyclicConnectedGraphOperators: List[MutationOp] = List(
    //TODO add/remove acyclic vertex/edge
    InlineNonTerminal,
    ExtractNonTerminal,
    ReuseNonTerminal
  )

  def mutateDirected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    mutate(grammar, directedGraphOperators, random, n)
  }

  def mutateDirectedConnected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    mutate(grammar, directedConnectedGraphOperators, random, n,
      (g: Grammar[V, E]) => g.expand.isConnected,
      s"not connected: ${grammar.expand}")
  }

  def mutateAcyclicConnected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    mutate(grammar, directedConnectedGraphOperators, random, n,
      (g: Grammar[V, E]) => g.expand.isConnected && !g.expand.hasCycle)
  }

  private def mutate[V, E](
    grammar:        Grammar[V, E],
    operators:      List[MutationOp],
    random:         Random,
    n:              Int,
    invariant:      Grammar[V, E] => Boolean = (_: Grammar[V, E]) => true,
    invariantError: => String                = ""
  ): Grammar[V, E] = {
    assert(invariant(grammar), invariantError)

    var current = grammar
    var i = 0
    while (i < n) {
      // println(s"mutate $i")
      //TODO: only select from operators which are possible to apply
      val operator = random.select(operators)
      val resultOption = operator(current, random)
      resultOption foreach { newGrammar =>
        assert(invariant(newGrammar), s"\nbefore ${operator.getClass.getName}:\n$current\nexpanded: ${current.expand}\nafter ${operator.getClass.getName}: $invariantError\n${newGrammar}\nexpanded: ${newGrammar.expand}")
        current = newGrammar
        i += 1
      }
    }

    current.cleanup
  }
}
