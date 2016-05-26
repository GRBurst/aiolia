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
) extends NeuralNetworkGrammarOpConfig { config =>

  val inputs = Brain.inputs
  val outputs = Brain.outputs

  val random = Random(0)

  val neuronMutationStrength = 0.1
  val synapseMutationStrength = 0.4
  val mutationOperators: List[(Grammar[Double, Double]) => Option[Grammar[Double, Double]]] = (
    1 -> AddConnectedVertex(config) ::
    20 -> AddEdge(config) ::
    1 -> MutateVertex(config) ::
    1 -> MutateEdge(config) ::
    0 -> AddAcyclicEdge(config) ::
    1 -> RemoveInterconnectedEdge(config) ::
    5 -> SplitEdge(config) ::
    0 -> ReconnectEdge(config) ::
    0 -> Shrink(config) ::
    // 1 -> ExtractNonTerminal(config) ::
    // 1 -> ReuseNonTerminalAcyclic(config) ::
    // 1 -> InlineNonTerminal(config) ::
    Nil
  ).flatMap{ case (n, op) => List.fill(n)(op) }
}

object Zoo extends App {
  val zoo = new LivingZoo(new ZooConfig)
  zoo.live()
}
