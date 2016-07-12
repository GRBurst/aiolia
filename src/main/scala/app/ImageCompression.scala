package aiolia.app

import aiolia.geneticAlgorithm._
import aiolia.grammar._
import aiolia.neuralNetwork._
import aiolia.util.{DOTExport, _}
import aiolia.graph.DSL._

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object ImageCompression extends App {
  val ga = GeneticAlgorithm(ImageCompressionConfig())
  ga.runFor(Duration.Inf)
}

case class ImageCompressionConfig(
  override val populationSize: Int = 24,
  override val tournamentSize: Int = 5,
  mutationCountPerElement: Double = 0.0251,
  mutationGaussianScale: Double = 0.1833,
  override val neuronMutationStrength: Double = 0.1,
  override val synapseMutationStrength: Double = 0.4602,
  elementCountPenalty: Double = 0.00000079,
  override val addAcyclicEdgeFreq: Int = 189,
  override val removeInterconnectedEdgeFreq: Int = 80,
  override val splitEdgeFreq: Int = 82,
  override val reconnectEdgeFreq: Int = 270,
  override val shrinkFreq: Int = 73,
  override val mutateVertexFreq: Int = 85,
  override val mutateEdgeFreq: Int = 284,

  seed: Any = 0,
  override val parallel: Boolean = true,
  override val prefix: String = "",
  override val nested: Boolean = false,
  preview: Boolean = true,
  picture: String = "primitives.png",
  pictureWidth: Int = 16
) extends Config[Grammar[Double, Double]] with FeedForwardNetworkConfig {
  config =>
  override def toString = "IC(p: %d, ts: %d, mut#: %6.4f, mutSc: %6.4f, mutv: %6.4f, mute: %6.4f, pen:%10.8f, freq: %d %d %d %d %d %d %d)" format (populationSize, tournamentSize, mutationCountPerElement, mutationGaussianScale, neuronMutationStrength, synapseMutationStrength, elementCountPenalty, addAcyclicEdgeFreq, removeInterconnectedEdgeFreq, splitEdgeFreq, reconnectEdgeFreq, shrinkFreq, mutateVertexFreq, mutateEdgeFreq)
  type Genotype = Grammar[Double, Double]
  type Phenotype = Image

  override def mutationCount(g: Genotype) = random.nextGaussian(mutationGaussianScale * mutationCountPerElement * g.expand.vertices.size, mutationCountPerElement * g.expand.vertices.size).toInt.max(1)

  val compilePixelThreshold = 1000000

  val target = Image.readResource("/" + picture).resized(pictureWidth)

  def log2(x: Double) = Math.log(x) / Math.log(2)

  val steps = log2(target.w).ceil.toInt
  lazy val resizedTargets = (0 to steps).map(1 << _).map(target.resized(_))
  // Future { target.write("/tmp/currentresized.png") }

  val inputs = VL(0, 1)
  val outputs = VL(2, 3, 4)
  val baseGenotype = Grammar.inOut(inputs, outputs, () => Some(0.0))

  def calculateFitness(g: Genotype, prefix: String): Double = {
    var sum = 0.0
    sum -= imageDistance(g, target, prefix)
    // sum -= resizedTargets.map(t => imageDistance(g, t, prefix)).sum
    sum -= g.expand.numElements.toDouble * elementCountPenalty
    // sum += Math.log(1 + Math.log(1 + g.compressionRatio)) * 0.1
    // sum -= (g.expand.vertices -- V(2)).count(g.expand.outDegree(_) > 0) * 0.001
    // sum -= g.expand.connectedComponents.size * 0.01
    sum
  }

  def imageDistance(g: Genotype, target: Image, prefix: String = ""): Double = generateImage(g, target.w, target.h, prefix) fuzzyDistance target

  def generateImage(g: Genotype, w: Int, h: Int, prefix: String = "") = {
    val network = FeedForward(inputs, outputs, g.expand)
    val image = Image.create(w, h)

    if (image.pixels >= compilePixelThreshold) {
      // if (!nested && prefix.nonEmpty) log(s"$prefix compiling...")
      network.compile()
    }

    // if (!nested && prefix.nonEmpty) log(s"$prefix network.compute...")
    val f = (x: Double, y: Double) => network.compute(Array(x, y))
    image fill f
  }

  override def stats(best: Genotype) = s", width: ${target.w} (${target.pixels}px), dst: ${"%6.4f" format imageDistance(best, target)}, el: ${best.numElements}, comp: ${"%4.2f" format best.compressionRatio}, rules: ${best.productions.size}, components: ${best.expand.connectedComponents.size}"

  var nextDraw = Duration.Zero.fromNow
  override def afterFitness(_population: Population, fitness: (Genotype) => Double, generation: Int) {
    if (preview && nextDraw.timeLeft <= Duration.Zero) {
      nextDraw = 1 seconds fromNow
      Future {
        val population = _population.sortBy(fitness).reverse
        // val best = population.head
        //   // generateImage(best, target.w, target.h).write(s"/tmp/current.png")
        //   // File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, inputs, outputs))
        //   // File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))
        drawPopulation(population, "/tmp/population.png")
      }
    }
  }

  def drawPopulation(population: Population, filename: String) {
    val size = Math.sqrt(1 + populationSize).ceil.toInt
    val scale = 1.0 //512.0 / (size * target.w)
    val w = (target.w * scale).toInt
    val h = (target.h * scale).toInt
    val im = Image.create(size * w, size * h)
    for ((gim, i) <- (target.resized(w) :: population.map(g => generateImage(g, w, h))).zipWithIndex) {
      val x = i % size
      val y = i / size
      im.insert(gim, x * w, y * h)
    }
    im.write(filename)
  }
}
