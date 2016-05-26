package aiolia.app

import aiolia.geneticAlgorithm._
import scala.concurrent.duration._

object ImageCompressionMeta extends App {
  try { assert(false) } catch { case ae: AssertionError => println("assertions activated") }
  val fitnessComputations = 16000
  val metaPopulationSize = 12
  val pictureWidth = 16
  val pictures = List("fruits.jpg", "primitives.png")
  val seeds = List[Any](0, 1) //, "penos")

  val metaGenerations = 1000
  val metaMaxMutationCount = 1.0

  val fitnessComputationsPerGen = fitnessComputations * metaPopulationSize * pictures.size * seeds.size
  println(s"fitness computations per meta generation: ${fitnessComputationsPerGen}")
  // create space separated values from output:
  // cat aiolia.log | grep "fit:" | sed "s/[^ ]*://g" | sed 's/[,)(\[\]//g' | sed 's/\]//g' > aiolia.log.tsv

  val metaConfig = new Config[ImageCompressionConfig] {
    type G = ImageCompressionConfig
    val seed = 0
    override val parallel = true
    override val populationSize = metaPopulationSize
    override val tournamentSize = 2
    override def mutationCount(g: G) = (metaMaxMutationCount + random.r.nextGaussian).toInt.max(1)
    val baseGenotype = new ImageCompressionConfig(parallel = true, nested = true, preview = false, pictureWidth = pictureWidth)
    def calculateFitness(g: G, prefix: String) = {
      val fit = (for (picture <- pictures; seed <- seeds) yield {
        val ga = GeneticAlgorithm(g.copy(prefix = prefix, seed = seed, picture = picture))
        val best = ga.runForFitnessComputations(fitnessComputations)
        -g.imageDistance(best, g.target, "")
      }).sum / (pictures.size * seeds.size)
      logln(s"$prefix${"%6.4f" format fit} ${g.toString}")
      fit
    }
    val r = random.r
    def lognorm() = Math.exp(r.nextGaussian * 0.5) // 80% in range [0.5, 2]
    def m(x: Double): Double = x * lognorm()
    def m(x: Int): Int = (x * lognorm()).round.toInt
    override val mutationOperators = (
      ((icc: G) => { val p = 2.max(m(icc.populationSize)); Some(icc.copy(populationSize = p, tournamentSize = p min icc.tournamentSize)) }) ::
      ((icc: G) => Some(icc.copy(tournamentSize = 1.max(m(icc.tournamentSize)).min(icc.populationSize)))) ::
      ((icc: G) => Some(icc.copy(mutationCountPerElement = m(icc.mutationCountPerElement)))) ::
      ((icc: G) => Some(icc.copy(mutationGaussianScale = m(icc.mutationGaussianScale)))) ::
      ((icc: G) => Some(icc.copy(neuronMutationStrength = m(icc.neuronMutationStrength)))) ::
      ((icc: G) => Some(icc.copy(synapseMutationStrength = m(icc.synapseMutationStrength)))) ::
      ((icc: G) => Some(icc.copy(elementCountPenalty = m(icc.elementCountPenalty)))) ::
      ((icc: G) => Some(icc.copy(addAcyclicEdgeFreq = 0 max m(icc.addAcyclicEdgeFreq)))) ::
      ((icc: G) => Some(icc.copy(removeInterconnectedEdgeFreq = 0 max m(icc.removeInterconnectedEdgeFreq)))) ::
      ((icc: G) => Some(icc.copy(splitEdgeFreq = 0 max m(icc.splitEdgeFreq)))) ::
      ((icc: G) => Some(icc.copy(reconnectEdgeFreq = 0 max m(icc.reconnectEdgeFreq)))) ::
      ((icc: G) => Some(icc.copy(shrinkFreq = 0 max m(icc.shrinkFreq)))) ::
      ((icc: G) => Some(icc.copy(mutateVertexFreq = 0 max m(icc.mutateVertexFreq)))) ::
      ((icc: G) => Some(icc.copy(mutateEdgeFreq = 0 max m(icc.mutateEdgeFreq)))) ::
      Nil
    )
    override def stats(g: G) = s"    [${"%4d" format (fitnessComputations / g.populationSize)}] ${g.toString}\n"
  }

  GeneticAlgorithm(metaConfig).runFor(metaGenerations)
}
