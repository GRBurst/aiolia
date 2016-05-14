package aiolia.app

import aiolia.graph._
import aiolia.graph.types._
import aiolia.util._
import aiolia.graph.dsl._

object Main extends App {
  println("Good Bye Aiolia")

  // val grammar = Mutation.mutateDirectedConnected(Grammar.minimal, Random(3), 100)
  // println(grammar.uniqueVertices)
  // File.write("grammar.dot", DOTExport.toDOT(grammar))
  // File.write("graph.dot", DOTExport.toDOT(grammar.expand))

  for (seed <- -5 to 325) try {
    println(seed)
    // val digraph = new DirectedGraphMutation(seed)
    // println(Mutation.mutate(Grammar.minimal, digraph, 100))
    // val mut = new BugHunter(seed)
    // println(Mutation.mutate(Grammar.minimal, mut, 100))
  }
  catch {
    // case _@ IsotopicException =>
    case e: AssertionError => {
      println("\n\nASSERTION\n" + e.getMessage + "\n" + e.getStackTrace.mkString("\n"))
      Thread.sleep(2000)
    }
  }

}
