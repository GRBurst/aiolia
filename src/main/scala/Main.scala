package aiolia

import aiolia._
import aiolia.export.DOTExport
import aiolia.graph._
import aiolia.graph.types._
import aiolia.graph.dsl._
import aiolia.helpers._

object Main extends App {
  println("Good Bye Aiolia")

  val grammar = Mutation.mutateDirectedConnected(Grammar.minimal, Random(0), 100)
  println(grammar.uniqueVertices)
  write("grammar.dot", DOTExport.toDOT(grammar))
  write("graph.dot", DOTExport.toDOT(grammar.expand))

  def write(filename: String, content: String) {
    Some(new java.io.PrintWriter(filename)).foreach{ p =>
      p.write(content)
      p.close
    }
  }
}
