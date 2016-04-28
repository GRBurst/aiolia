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
    assert(grammar.expand.isConnected, s"not connected: ${grammar.expand}")
    val newGrammar = mutate(grammar, directedConnectedGraphOperators, random, n)
    assert(newGrammar.expand.isConnected)
    newGrammar
  }

  def mutateAcyclicConnected[V, E](grammar: Grammar[V, E], random: Random, n: Int = 1) = {
    assert(grammar.expand.isConnected)
    assert(!grammar.expand.hasCycle)
    val newGrammar = mutate(grammar, directedConnectedGraphOperators, random, n)
    assert(newGrammar.expand.isConnected)
    assert(!newGrammar.expand.hasCycle)
    newGrammar
  }

  private def mutate[V, E](grammar: Grammar[V, E], operators: List[MutationOp], random: Random, n: Int): Grammar[V, E] = {
    //TODO: assert constraints after every mutation
    var current = grammar
    var i = 0
    while (i < n) {
      // println(s"mutate $i")
      //TODO: only select from operators which are possible to apply
      val operator = random.select(operators)
      println(operator)
      val resultOption = operator(current, random)
      resultOption foreach { newGrammar =>
        current = newGrammar
        assert(current.expand.isConnected, s"not connected: ${current.expand}") //TODO: this assert is temporary, remove it later
        i += 1
      }
    }
    current.cleanup
  }
}
