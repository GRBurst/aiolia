package aiolia.app.zoo

import world.Brain
import aiolia.grammar._
import aiolia.util._

class ZooConfig(
    val mutationCount:     Int    = 10,
    val foodProbability:   Double = 0.2,
    val minimumPopulation: Int    = 5,
    val initialEnergy:     Double = 0.8,
    val walkEffort:        Double = 0.005,
    val thinkEffort:       Double = 0.0001,
    val worldDimensions:   Vec2   = Vec2(30, 30)
) extends InOutGrammarOpConfig[Double, Double] { config =>

  val inputs = Brain.inputs
  val outputs = Brain.outputs

  val random = Random(0)

  val mutationOperators: List[(Grammar[Double, Double]) => Option[Grammar[Double, Double]]] = (
    1 -> AddConnectedVertex(config) ::
    1 -> AddEdge(config) ::
    1 -> MutateVertex(config) ::
    1 -> MutateEdge(config) ::
    1 -> AddAcyclicEdge(config) ::
    1 -> RemoveInterconnectedEdge(config) ::
    1 -> SplitEdge(config) ::
    1 -> ReconnectEdge(config) ::
    0 -> Shrink(config) ::
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
