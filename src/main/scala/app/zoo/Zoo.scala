package aiolia.app.zoo

import aiolia.grammar._
import aiolia.util._

class ZooConfig(
  val mutationCount: Int = 20,
  val foodProbability: Double = 0.5,
  val minimumPopulation: Int = 10,
  val initialEnergy: Double = 0.8,
  val walkEffort: Double = 0.001,
  val thinkEffort: Double = 0.0005,
  val worldDimensions: Vec2 = Vec2(10,10)
) extends DataGraphGrammarOpConfig[Double, Double] { config =>

  val random = Random(0)

  val mutationOperators: List[(Grammar[Double,Double]) => Option[Grammar[Double,Double]]] = (
    2 -> AddConnectedVertex(config) ::
    2 -> AddEdge(config) ::
    1 -> MutateVertex(config) ::
    1 -> MutateEdge(config) ::
    // 1 -> ExtractNonTerminal(config) ::
    // 1 -> ReuseNonTerminalAcyclic(config) ::
    // 1 -> InlineNonTerminal(config) ::
    Nil
  ).flatMap{ case (n, op) => List.fill(n)(op) }

  override def initVertexData() = Some(random.r.nextDouble * 2 - 1)
  override def initEdgeData() = Some(random.r.nextDouble * 2 - 1)
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian
}

object Zoo extends App {
  val zoo = new LivingZoo(new ZooConfig)
  zoo.live()
}

