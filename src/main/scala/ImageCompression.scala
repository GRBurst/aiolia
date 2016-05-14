package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers._
import aiolia.graph.dsl._
import aiolia.export.DOTExport
import scala.concurrent.duration._
import aiolia.mutations._

object ImageCompression extends App {
  val ga = GeneticAlgorithm(ImageCompressionConfig)
  ga.runFor(5 minutes)
}

trait DataGraphGrammarOpConfig[V, E] extends MutationOpConfig[Grammar[V, E]] {
  def initVertexData(): Option[V]
  def initEdgeData(): Option[E]
  def mutateVertexData(d: V): V
  def mutateEdgeData(d: E): E
}

trait FeedForwardGrammarOpConfig extends DataGraphGrammarOpConfig[Double, Double] {
  val feedForwardInputs: List[Vertex]
  val feedForwardOutputs: List[Vertex]
}

trait GeneticAlgorithmFeedForwardConfig extends GeneticAlgorithmConfig[Grammar[Double, Double]] with FeedForwardGrammarOpConfig { config =>

  val mutationOperators = (
    1 -> AddConnectedVertex(config) ::
    1 -> MutateVertex(config) ::
    1 -> RemoveVertex(config) ::

    3 -> AddAcyclicEdge(config) ::
    1 -> MutateEdge(config) ::
    1 -> RemoveEdge(config) ::

    0 -> ExtractNonTerminal(config) ::
    0 -> ReuseNonTerminalAcyclic(config) ::
    0 -> InlineNonTerminal(config) ::
    //TODO? RemoveNonTerminal

    Nil
  ).flatMap{ case (n, op) => List.fill(n)(op) }

  override def afterMutationOp(g: Grammar[Double, Double]) = g.cleanup

  override def initVertexData() = Some(random.r.nextGaussian)
  override def initEdgeData() = Some(random.r.nextGaussian)
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian * 0.01
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian * 0.01
  override def genotypeInvariant(grammar: Grammar[Double, Double]): Boolean = (
    !grammar.expand.hasCycle &&
    feedForwardInputs.forall(grammar.expand.inDegree(_) == 0) &&
    feedForwardOutputs.forall(grammar.expand.outDegree(_) == 0)
  )
}

object ImageCompressionConfig extends GeneticAlgorithmFeedForwardConfig { config =>
  type Genotype = Grammar[Double, Double]
  type Phenotype = Image

  val seed = 0
  override val populationSize = 100
  val compilePixelThreshold = 200000
  override val mutationCount = 4

  val target = Image.read("apple.jpg").resized(128)
  def log2(x: Double) = Math.log(x) / Math.log(2)
  val steps = log2(target.w).ceil.toInt
  val resizedTargets = (0 to steps).map(1 << _).map(target.resized(_))
  target.write("/tmp/currentresized.png")

  val feedForwardInputs = VL(0, 1)
  val feedForwardOutputs = VL(2, 3, 4)
  val baseGenotype = Grammar.feedForward(feedForwardInputs, feedForwardOutputs)

  def calculateFitness(g: Genotype, prefix: String): Double = {
    var sum = 0.0
    sum -= resizedTargets.map(t => imageDistance(g, t, prefix)).sum
    sum -= g.numElements.toDouble * 0.000005
    // sum += Math.log(1 + Math.log(1 + g.compressionRatio)) * 0.1
    // sum -= (g.expand.vertices -- V(2)).count(g.expand.outDegree(_) > 0) * 0.001
    sum -= g.expand.connectedComponents.size * 0.01
    sum
  }

  def imageDistance(g: Genotype, target: Image, prefix: String = ""): Double = generateImage(g, target.w, target.h, prefix) distance target

  def generateImage(g: Genotype, w: Int, h: Int, prefix: String = "") = {
    val network = FeedForwardNeuralNetwork(feedForwardInputs, feedForwardOutputs, g.expand)
    val image = Image.create(w, h)

    if (image.pixels >= compilePixelThreshold) {
      if (prefix.nonEmpty) print(s"$prefix compiling...      ")
      network.compile()
    }

    if (prefix.nonEmpty) print(s"$prefix fitness...      ")
    image fill network.compute
  }

  override def stats(best: Genotype) = s"width: ${target.w} (${target.pixels}px), dst: ${"%6.4f" format imageDistance(best, target)}, el: ${best.numElements}, comp: ${"%4.2f" format (best.compressionRatio)}, rules: ${best.productions.size}, components: ${best.expand.connectedComponents.size}"

  override def afterFitness(population: Population) {
    val best = population.head
    print("\rpreviews...     ")
    generateImage(best, target.w, target.h).write(s"/tmp/current.png")
    File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs, feedForwardOutputs))
    File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))
  }
}
