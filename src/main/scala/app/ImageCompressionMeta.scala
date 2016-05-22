package aiolia.app

import aiolia.geneticAlgorithm._
import scala.concurrent.duration._

object ImageCompressionMeta extends App {
  try { assert(false) } catch { case ae: AssertionError => println("assertions activated") }
  val fitnessComputations = 8000
  val metaPopulationSize = 8
  val pictureWidth = 32
  val metaGenerations = 1000
  val metaMaxMutationCount = 2
  val pictures = List("fruits.jpg", "primitives.png")
  println(s"fitness computations per meta generation: ${fitnessComputations * metaPopulationSize * pictures.size}")

  val metaConfig = new Config[ImageCompressionConfig] {
    type G = ImageCompressionConfig
    val seed = 0
    override val parallel = true
    override val populationSize = metaPopulationSize
    override val tournamentSize = 2
    override def mutationCount(g: G) = (random.r.nextGaussian.toInt + metaMaxMutationCount).max(1)
    val baseGenotype = new ImageCompressionConfig(parallel = true, nested = true, preview = false, pictureWidth = pictureWidth)
    def calculateFitness(g: G, prefix: String) = {
      val fit = pictures.par.map{ picture =>
        val ga = GeneticAlgorithm(g.copy(prefix = prefix, picture = picture))
        val best = ga.runForFitnessComputations(fitnessComputations)
        -g.imageDistance(best, g.target, "")
      }.sum / pictures.size
      logln(s"$prefix${"%6.4f" format fit} ${g.toString}")
      fit
    }
    val r = random.r
    def m(x: Double): Double = x * Math.exp(r.nextGaussian)
    def m(x: Int): Int = { val res = (x * Math.exp(r.nextGaussian)).round.toInt; if (res == x) x + 1 else res }
    override val mutationOperators = (
      ((icc: G) => Some(icc.copy(populationSize = 2.max(m(icc.populationSize))))) ::
      ((icc: G) => Some(icc.copy(tournamentSize = 2.max(m(icc.tournamentSize))))) ::
      ((icc: G) => Some(icc.copy(mutationCountPerElement = m(icc.mutationCountPerElement)))) ::
      ((icc: G) => Some(icc.copy(mutationGaussianScale = m(icc.mutationGaussianScale)))) ::
      ((icc: G) => Some(icc.copy(vertexMutationStrength = m(icc.vertexMutationStrength)))) ::
      ((icc: G) => Some(icc.copy(edgeMutationStrength = m(icc.edgeMutationStrength)))) ::
      ((icc: G) => Some(icc.copy(elementCountPenalty = m(icc.elementCountPenalty)))) ::
      // ((icc: G) => Some(icc.copy(addAcyclicEdgeFreq = 0.max(m(icc.addAcyclicEdgeFreq))))) ::
      // ((icc: G) => Some(icc.copy(removeInterconnectedEdgeFreq = 0.max(m(icc.removeInterconnectedEdgeFreq))))) ::
      // ((icc: G) => Some(icc.copy(splitEdgeFreq = 0.max(m(icc.splitEdgeFreq))))) ::
      // ((icc: G) => Some(icc.copy(reconnectEdgeFreq = 0.max(m(icc.reconnectEdgeFreq))))) ::
      // ((icc: G) => Some(icc.copy(shrinkFreq = 0.max(m(icc.shrinkFreq))))) ::
      // ((icc: G) => Some(icc.copy(mutateVertexFreq = 0.max(m(icc.mutateVertexFreq))))) ::
      // ((icc: G) => Some(icc.copy(mutateEdgeFreq = 0.max(m(icc.mutateEdgeFreq))))) ::
      Nil
    )
    override def stats(g: G) = s"    [${"%4d" format (fitnessComputations / g.populationSize)}] ${g.toString}\n"
  }

  GeneticAlgorithm(metaConfig).runFor(metaGenerations)
}
