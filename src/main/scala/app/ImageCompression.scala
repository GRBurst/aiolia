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
  ga.runFor(5)
}

case class ImageCompressionConfig(
    override val populationSize:  Int    = 10,
    override val tournamentSize:  Int    = 3,
    mutationCountPerElement:      Double = 0.1,
    vertexMutationStrength:       Double = 0.1,
    edgeMutationStrength:         Double = 0.1,
    elementCountPenalty:          Double = 0.0000001,
    addAcyclicEdgeFreq:           Int    = 1,
    removeInterconnectedEdgeFreq: Int    = 1,
    splitEdgeFreq:                Int    = 1,
    reconnectEdgeFreq:            Int    = 1,
    shrinkFreq:                   Int    = 1,
    mutateVertexFreq:             Int    = 1,
    mutateEdgeFreq:               Int    = 1,

    override val parallel: Boolean = true,
    override val prefix:   String  = "",
    override val nested:   Boolean = false,
    preview:               Boolean = true
) extends Config[Grammar[Double, Double]] with FeedForwardGrammarOpConfig {
  config =>
  override def toString = "IC(p: %d, ts: %d, mut#: %6.4f, mutv: %6.4f, mute: %6.4f, pen:%9.7f, freq: %d %d %d %d %d %d %d)" format (populationSize, tournamentSize, mutationCountPerElement, vertexMutationStrength, edgeMutationStrength, elementCountPenalty, addAcyclicEdgeFreq, removeInterconnectedEdgeFreq, splitEdgeFreq, reconnectEdgeFreq, shrinkFreq, mutateVertexFreq, mutateEdgeFreq)
  type Genotype = Grammar[Double, Double]
  type Phenotype = Image

  override def mutationCount(g: Genotype): Int = (mutationCountPerElement * g.expand.vertices.size).toInt + 1

  val mutationOperators = (
    addAcyclicEdgeFreq -> AddAcyclicEdge(config) ::
    removeInterconnectedEdgeFreq -> RemoveInterconnectedEdge(config) ::
    splitEdgeFreq -> SplitEdge(config) ::
    reconnectEdgeFreq -> ReconnectEdge(config) ::
    shrinkFreq -> Shrink(config) ::
    mutateVertexFreq -> MutateVertex(config) ::
    mutateEdgeFreq -> MutateEdge(config) ::
    // 1 -> ExtractNonTerminal(config) ::
    // 1 -> ReuseNonTerminalAcyclic(config) ::
    // 1 -> InlineNonTerminal(config) ::
    Nil
  ).flatMap{ case (n, op) => List.fill(n)(op) }

  override def afterMutationOp(g: Grammar[Double, Double]) = g.cleanup

  override def initVertexData() = Some(random.r.nextDouble * 2 - 1)
  override def initEdgeData() = Some(random.r.nextDouble * 2 - 1)
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian * vertexMutationStrength
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian * edgeMutationStrength
  override def genotypeInvariant(grammar: Grammar[Double, Double]): Boolean = !grammar.expand.hasCycle &&
    feedForwardInputs.forall(grammar.expand.inDegree(_) == 0) &&
    feedForwardOutputs.forall(grammar.expand.outDegree(_) == 0) &&
    (grammar.expand.vertices -- feedForwardInputs -- feedForwardOutputs).forall(v => grammar.expand.inDegree(v) > 0 && grammar.expand.outDegree(v) > 0)

  val seed = 0
  val compilePixelThreshold = 1000000

  val target = Image.readResource("/fruits.jpg").resized(32)

  def log2(x: Double) = Math.log(x) / Math.log(2)

  val steps = log2(target.w).ceil.toInt
  lazy val resizedTargets = (0 to steps).map(1 << _).map(target.resized(_))
  // Future { target.write("/tmp/currentresized.png") }

  val feedForwardInputs = VL(0, 1)
  val feedForwardOutputs = VL(2, 3, 4)
  val baseGenotype = Grammar.feedForward(feedForwardInputs, feedForwardOutputs)

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
    val network = FeedForward(feedForwardInputs, feedForwardOutputs, g.expand)
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
  override def afterFitness(_population: Population, fitness: (Genotype) => Double) {
    if (preview && nextDraw.timeLeft <= Duration.Zero) {
      nextDraw = 1 seconds fromNow
      Future {
        val population = _population.sortBy(fitness).reverse
        // val best = population.head
        //   // generateImage(best, target.w, target.h).write(s"/tmp/current.png")
        //   // File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs, feedForwardOutputs))
        //   // File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))
        drawPopulation(population, "/tmp/population.png")
      }
    }
  }

  def drawPopulation(population: Population, filename: String) {
    val size = Math.sqrt(1 + populationSize).ceil.toInt
    val scale = 512.0 / (size * target.w)
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
