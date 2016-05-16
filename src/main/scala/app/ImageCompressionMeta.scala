package aiolia.app

import aiolia.geneticAlgorithm._
import scala.concurrent.duration._

object ImageCompressionMeta extends App {
  val runDuration = 1 minute
  val metaPopulationSize = 5
  val metaDuration = Duration.Inf

  val metaConfig = new Config[ImageCompressionConfig] {
    type G = ImageCompressionConfig
    val seed = 0
    override val parallel = false
    override val populationSize = metaPopulationSize
    override val tournamentSize = 2
    override def mutationCount(g: G) = 1
    val baseGenotype = new ImageCompressionConfig(parallel = true, nested = true)
    def calculateFitness(g: G, prefix: String) = {
      val ga = GeneticAlgorithm(g.copy(prefix = prefix))
      val best = ga.runFor(runDuration)
      println()
      -g.imageDistance(best, g.target, "")
    }
    val r = random.r
    def m(x: Double): Double = x * (r.nextDouble * 1.5 + 0.5)
    def m(x: Int): Int = (x * (r.nextDouble * 1.5 + 0.5)).round.toInt
    override val mutationOperators = (
      ((icc: G) => Some(icc.copy(populationSize = 2.max(m(icc.populationSize))))) ::
      ((icc: G) => Some(icc.copy(tournamentSize = 2.max(m(icc.tournamentSize))))) ::
      ((icc: G) => Some(icc.copy(mutationCountPerElement = m(icc.mutationCountPerElement)))) ::
      ((icc: G) => Some(icc.copy(vertexMutationStrength = m(icc.vertexMutationStrength)))) ::
      ((icc: G) => Some(icc.copy(edgeMutationStrength = m(icc.edgeMutationStrength)))) ::
      ((icc: G) => Some(icc.copy(elementCountPenalty = m(icc.elementCountPenalty)))) ::
      ((icc: G) => Some(icc.copy(addAcyclicEdgeFreq = 0.max(m(icc.addAcyclicEdgeFreq))))) ::
      ((icc: G) => Some(icc.copy(removeInterconnectedEdgeFreq = 0.max(m(icc.removeInterconnectedEdgeFreq))))) ::
      ((icc: G) => Some(icc.copy(splitEdgeFreq = 0.max(m(icc.splitEdgeFreq))))) ::
      ((icc: G) => Some(icc.copy(reconnectEdgeFreq = 0.max(m(icc.reconnectEdgeFreq))))) ::
      ((icc: G) => Some(icc.copy(shrinkFreq = 0.max(m(icc.shrinkFreq))))) ::
      ((icc: G) => Some(icc.copy(mutateVertexFreq = 0.max(m(icc.mutateVertexFreq))))) ::
      ((icc: G) => Some(icc.copy(mutateEdgeFreq = 0.max(m(icc.mutateEdgeFreq))))) ::
      Nil
    )
    override def stats(g: G) = g.toString
  }

  GeneticAlgorithm(metaConfig).runFor(metaDuration)
}
