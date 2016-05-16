package aiolia.app

import aiolia.geneticAlgorithm._
import aiolia.grammar._
import aiolia.neuralNetwork._
import aiolia.util.{DOTExport, _}
import aiolia.graph.DSL._

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object ImageCompressionMeta extends App {
  val metaConfig = new Config[ImageCompressionConfig] {
    type G = ImageCompressionConfig
    val seed = 0
    def r = random.r
    override val parallel = false
    override val populationSize: Int = 10
    override val tournamentSize = 2
    override def mutationCount(g: G) = 1
    val baseGenotype = ImageCompressionConfig(log = false)
    def calculateFitness(g: G, prefix: String) = g.calculateFitness(GeneticAlgorithm(g).runFor(2 minutes), prefix)
    override val mutationOperators = (
      ((icc: G) => Some(icc.copy(populationSize = 2.max((icc.populationSize * (r.nextDouble * 1.5 + 0.5)).toInt)))) ::
      ((icc: G) => Some(icc.copy(mutationCountPerElement = icc.mutationCountPerElement * (r.nextDouble * 1.5 + 0.5)))) ::
      ((icc: G) => Some(icc.copy(vertexMutationStrength = icc.vertexMutationStrength * (r.nextDouble * 1.5 + 0.5)))) ::
      ((icc: G) => Some(icc.copy(edgeMutationStrength = icc.edgeMutationStrength * (r.nextDouble * 1.5 + 0.5)))) ::
      ((icc: G) => Some(icc.copy(elementCountPenalty = icc.elementCountPenalty * (r.nextDouble * 1.5 + 0.5)))) ::
      ((icc: G) => Some(icc.copy(addAcyclicEdgeFreq = 0.max((icc.addAcyclicEdgeFreq * (r.nextDouble * 1.5 + 0.5)).toInt)))) ::
      ((icc: G) => Some(icc.copy(removeInterconnectedEdgeFreq = 0.max((icc.removeInterconnectedEdgeFreq * (r.nextDouble * 1.5 + 0.5)).toInt)))) ::
      ((icc: G) => Some(icc.copy(splitEdgeFreq = 0.max((icc.splitEdgeFreq * (r.nextDouble * 1.5 + 0.5)).toInt)))) ::
      ((icc: G) => Some(icc.copy(reconnectEdgeFreq = 0.max((icc.reconnectEdgeFreq * (r.nextDouble * 1.5 + 0.5)).toInt)))) ::
      ((icc: G) => Some(icc.copy(shrinkFreq = 0.max((icc.shrinkFreq * (r.nextDouble * 1.5 + 0.5)).toInt)))) ::
      ((icc: G) => Some(icc.copy(mutateVertexFreq = 0.max((icc.mutateVertexFreq * (r.nextDouble * 1.5 + 0.5)).toInt)))) ::
      ((icc: G) => Some(icc.copy(mutateEdgeFreq = 0.max((icc.mutateEdgeFreq * (r.nextDouble * 1.5 + 0.5)).toInt)))) ::
      Nil
    )
    override def stats(g: G) = g.toString
  }
  println(GeneticAlgorithm(metaConfig).runFor(Duration.Inf))
}

object ImageCompression extends App {
  val ga = GeneticAlgorithm(ImageCompressionConfig())
  ga.runFor(15 seconds)
}

case class ImageCompressionConfig(
    override val populationSize:  Int     = 128,
    mutationCountPerElement:      Double  = 0.01,
    vertexMutationStrength:       Double  = 0.08,
    edgeMutationStrength:         Double  = 0.05,
    elementCountPenalty:          Double  = 5.3E-7,
    addAcyclicEdgeFreq:           Int     = 1,
    removeInterconnectedEdgeFreq: Int     = 1,
    splitEdgeFreq:                Int     = 1,
    reconnectEdgeFreq:            Int     = 1,
    shrinkFreq:                   Int     = 1,
    mutateVertexFreq:             Int     = 1,
    mutateEdgeFreq:               Int     = 1,
    override val log:             Boolean = true
) extends Config[Grammar[Double, Double]] with FeedForwardGrammarOpConfig {
  config =>
  type Genotype = Grammar[Double, Double]
  type Phenotype = Image

  override def mutationCount(g: Genotype): Int = 1.max((g.numElements * mutationCountPerElement).toInt)

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
  val compilePixelThreshold = 200000

  val target = Image.read("apple2.jpg").resized(64)

  def log2(x: Double) = Math.log(x) / Math.log(2)

  val steps = log2(target.w).ceil.toInt
  val resizedTargets = (0 to steps).map(1 << _).map(target.resized(_))
  // target.write("/tmp/currentresized.png")

  val feedForwardInputs = VL(0, 1)
  val feedForwardOutputs = VL(2, 3, 4)
  val baseGenotype = Grammar.feedForward(feedForwardInputs, feedForwardOutputs)

  def calculateFitness(g: Genotype, prefix: String): Double = {
    var sum = 0.0
    sum -= imageDistance(g, target, prefix)
    // sum -= resizedTargets.map(t => imageDistance(g, t, prefix)).zipWithIndex.map{ case (d, i) => d * (1 << i) }.sum
    sum -= g.expand.numElements.toDouble * elementCountPenalty
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

  var nextDraw = Duration.Zero.fromNow
  override def afterFitness(_population: Population, fitness: (Genotype) => Double) {
    if (nextDraw.timeLeft <= Duration.Zero) {
      nextDraw = 1 seconds fromNow
      Future {
        val population = _population.sortBy(fitness).reverse
        val best = population.head
        // generateImage(best, target.w, target.h).write(s"/tmp/current.png")
        // File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs, feedForwardOutputs))
        // File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))
        drawPopulation(population, "/tmp/population.png")
      }
    }
  }

  def drawPopulation(population: Population, filename: String) {
    val size = Math.sqrt(populationSize).ceil.toInt
    val im = Image.create(size * target.w, size * target.h)
    for ((g, i) <- population.zipWithIndex) {
      val x = i % size
      val y = i / size
      im.insert(generateImage(g, target.w, target.h), x * target.w, y * target.h)
    }
    im.write(filename)
  }
}
