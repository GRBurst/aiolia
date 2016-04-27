package aiolia

import aiolia._
import aiolia.graph._
import aiolia.graph.types._
import aiolia.graph.dsl._
import aiolia.helpers._

object Main extends App {
  println("Good Bye Aiolia")

  val dot = Mutation.mutate(Grammar.minimal, Random(12), 30).toDOT
  Some(new java.io.PrintWriter("grammar.dot")).foreach{ p => p.write(dot); p.close }
}
