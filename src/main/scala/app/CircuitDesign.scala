package aiolia.app

import aiolia.geneticAlgorithm._
import aiolia.grammar._
import aiolia.circuit._
import aiolia.util.{DOTExport, _}
import aiolia.graph.DSL._
import aiolia.graph._

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import java.security.MessageDigest

import boopickle.Default._

object CircuitDesign extends App {
  val ga = GeneticAlgorithm(CircuitDesignConfig())
  ga.runFor(Duration.Inf)
}

case class CircuitDesignConfig() extends Config[Graph[Nothing, Nothing]] with CircuitConfig {
  config =>
  type Genotype = Graph[Nothing, Nothing]
  type Phenotype = Image

  override val populationSize: Int = 200
  override val tournamentSize = 4
  override def mutationCount(g: Genotype) = random.nextInt(1, 4)
  // override def mutationCount(g: Genotype) = 1

  val seed = 0
  override val parallel: Boolean = false

  // val inputs = List.tabulate(128)(x => v(x))
  // val outputs = List.tabulate(1)(x => v(x + 128))
  val inputs = VL(0,1)
  val outputs = VL(2)
  // val baseGenotype = pickleFromFile("bestCircuit.boo").getOrElse(Graph(vertices = (inputs ++ outputs).toSet))
  val baseGenotype = Graph(vertices = (inputs ++ outputs).toSet)
  override def genotypeCleanup(g: Genotype) = Simplify.simplify(inputs, outputs, g)

  def byteToBoolArray(inBytes: Array[Byte]): Array[Boolean] = {
    val outBools = new Array[Boolean](inBytes.size * 8)
    for ((byte, index) <- inBytes zipWithIndex) {
      for (bit <- 0 until 8) {
        outBools(index * 8 + (7 - bit)) = ((byte >> bit) & 1) == 1
      }
    }
    outBools
  }

  // def calcMd5Sum(inBytes: Array[Byte]): Array[Byte] = {
  //   val md = MessageDigest.getInstance("MD5")
  //   md.update(inBytes)
  //   md.digest()
  // }

  // def genExamples(n: Int) = {
  //   val random = new scala.util.Random(seed)
  //   List.fill(n) {
  //     val in = new Array[Byte](16)
  //     random.nextBytes(in)
  //     val out = calcMd5Sum(in)
  //     byteToBoolArray(in) -> byteToBoolArray(out).take(1)
  //   }
  // }

  var nextDraw = Duration.Zero.fromNow
  override def afterFitness(_population: Population, fitness: (Genotype) => Double, generation: Int) {
    val population = _population.sortBy(fitness).reverse
    if (nextDraw.timeLeft <= Duration.Zero) {
      nextDraw = 3 seconds fromNow
      Future {
        val best = population.head
        File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best, inputs, outputs))
        pickleIntoFile(best, "bestCircuit.boo")
      }
    }

    // val bestFitness = fitness(population.head)
    // if (bestFitness >= examples.size * outputs.size) {
    //   exampleCount *= 2
    //   examples = genExamples(exampleCount)
    // }
  }

  def pickleIntoFile(graph: Genotype, file: String) {
    import java.io.File
    import java.io.FileOutputStream
    val channel = new FileOutputStream(new File(file), false).getChannel()
    val buf = Pickle.intoBytes((graph.vertices, graph.edges))
    channel.write(buf)
    channel.close()
  }

  def pickleFromFile(file: String): Option[Genotype] = {
    import java.io.File
    import java.io.FileInputStream
    import java.nio.ByteBuffer
    val f = new File(file)
    if (f.exists && !f.isDirectory) {
      val fis = new FileInputStream(file)
      val chan = fis.getChannel()
      val size = chan.size
      val buf = ByteBuffer.allocate(size.toInt)
      chan.read(buf)
      buf.rewind()
      chan.close()
      fis.close()
      val (vertices, edges) = Unpickle[(Set[Vertex], Set[Edge])].fromBytes(buf)
      Some(Graph(vertices, edges))
    } else None
  }

  // var exampleCount = 10240
  // var examples = genExamples(exampleCount)
  def inputExamples(n: Int): Seq[Array[Boolean]] = {
    assert(n >= 1)
    if (n == 1) List(Array(false), Array(true))
    else inputExamples(n - 1).map(false +: _) ++ inputExamples(n - 1).map(true +: _)
  }
  // val examples = inputExamples(3).map { e =>
  //   val a = e(0)
  //   val b = e(1)
  //   val c = e(2)
  //   e -> Array(!(!(a && b) && !(a && c)))
  // }
  val examples = (
    // Array(
    Array(false, false) -> Array(false) ::
    Array(false, true) -> Array(true) ::
    Array(true, false) -> Array(true) ::
    Array(true, true) -> Array(false) ::
    // Array(true) -> Array(false) ::
    // Array(false) -> Array(true) ::
    Nil
  )

  def calculateFitness(g: Genotype, prefix: String): Double = {
    val circuit = Circuit(inputs, outputs, g)
    // println("compiling...")
    // circuit.compile()
    // println("evaluating...")
    var score: Int = 0
    for ((in, shouldOut) <- examples) {
      val computedOut = circuit.compute(in)
      for ((computedOutBit, shouldOutBit) <- computedOut zip shouldOut) {
        score += (if (computedOutBit == shouldOutBit) 1 else 0)
      }
    }
    val circuitSizeScore = 1.0 / (g.vertices.size)
    (score + circuitSizeScore)
  }

  // override def stats(best: Genotype) = s"/ ${(examples.size * outputs.size)}[$exampleCount], gates: ${best.vertices.size - inputs.size}, e: ${best.edges.size}"
  override def stats(best: Genotype) = s"/ ${(examples.size * outputs.size)}, gates: ${best.vertices.size - inputs.size}, e: ${best.edges.size}"
}
