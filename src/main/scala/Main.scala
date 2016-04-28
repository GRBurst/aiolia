package aiolia

import aiolia._
import aiolia.graph._
import aiolia.graph.types._
import aiolia.graph.dsl._
import aiolia.helpers._

object Main extends App {
  println("Good Bye Aiolia")

  val grammar = Mutation.mutate(Grammar.minimal, Random(0), 10)
  println(grammar.uniqueVertices)
  write("grammar.dot", grammar.toDOT)
  write("graph.dot", grammar.expand.toDOT)

  def write(filename: String, content: String) {
    Some(new java.io.PrintWriter(filename)).foreach{ p =>
      p.write(content)
      p.close
    }
  }
}
