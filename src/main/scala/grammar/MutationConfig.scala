package aiolia.grammar

import aiolia.graph._
import aiolia.util.Random

trait MutationOpConfig[G] {
  val random: Random
}

trait DataGraphGrammarOpConfig[V, E] extends MutationOpConfig[Grammar[V, E]] {
  def initVertexData(): Option[V] = None
  def initEdgeData(): Option[E] = None
  def mutateVertexData(d: V): V = d
  def mutateEdgeData(d: E): E = d
}
