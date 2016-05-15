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
  val ga = GeneticAlgorithm(ImageCompressionConfig)
  ga.runFor(5 minutes)
}

object ImageCompressionConfig extends GeneticAlgorithmFeedForwardConfig {
  config =>
  type Genotype = Grammar[Double, Double]
  type Phenotype = Image

  val seed = 0
  override val populationSize = 200
  val compilePixelThreshold = 200000
  override val mutationCount = 2

  val target = Image.read("apple.jpg").resized(32)

  def log2(x: Double) = Math.log(x) / Math.log(2)

  val steps = log2(target.w).ceil.toInt
  val resizedTargets = (0 to steps).map(1 << _).map(target.resized(_))
  target.write("/tmp/currentresized.png")

  val feedForwardInputs = VL(0, 1)
  val feedForwardOutputs = VL(2, 3, 4)
  val baseGenotype = Grammar.feedForward(feedForwardInputs, feedForwardOutputs)

  def calculateFitness(g: Genotype, prefix: String): Double = {
    var sum = 0.0
    sum -= imageDistance(g, target, prefix)
    // sum -= resizedTargets.map(t => imageDistance(g, t, prefix)).zipWithIndex.map{ case (d, i) => d * (1 << i) }.sum
    sum -= g.expand.numElements.toDouble * 0.000001
    // sum += Math.log(1 + Math.log(1 + g.compressionRatio)) * 0.1
    // sum -= (g.expand.vertices -- V(2)).count(g.expand.outDegree(_) > 0) * 0.001
    // sum -= g.expand.connectedComponents.size * 0.01
    sum
  }

  def imageDistance(g: Genotype, target: Image, prefix: String = ""): Double = generateImage(g, target.w, target.h, prefix) distance target

  def generateImage(g: Genotype, w: Int, h: Int, prefix: String = "") = {
    val network = FeedForward(feedForwardInputs, feedForwardOutputs, g.expand)
    val image = Image.create(w, h)

    if (image.pixels >= compilePixelThreshold) {
      if (prefix.nonEmpty) print(s"$prefix compiling...      ")
      network.compile()
    }

    if (prefix.nonEmpty) print(s"$prefix fitness...      ")
    val f = (x: Double, y: Double) => network.compute(Array(x, y))
    image fill f
  }

  override def stats(best: Genotype) = s"width: ${target.w} (${target.pixels}px), dst: ${"%6.4f" format imageDistance(best, target)}, el: ${best.numElements}, comp: ${"%4.2f" format best.compressionRatio}, rules: ${best.productions.size}, components: ${best.expand.connectedComponents.size}"

  override def afterFitness(population: Population) {
    val best = population.head
    print("\rpreviews...     ")
    Future {
      generateImage(best, target.w, target.h).write(s"/tmp/current.png")
      // File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs, feedForwardOutputs))
      // File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))
    }
  }
}
