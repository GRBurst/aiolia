package aiolia.app

import aiolia.geneticAlgorithm._
import aiolia.grammar._
import aiolia.neuralNetwork._
import aiolia.util.{DOTExport, _}
import aiolia.graph.DSL._
import aiolia.graph.Graph

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import java.security.MessageDigest

object CircuitDesign extends App {
  val ga = GeneticAlgorithm(CircuitDesignConfig())
  ga.runFor(Duration.Inf)
}

case class CircuitDesignConfig() extends Config[Graph[Nothing, Nothing]] with CircuitConfig {
  config =>
  type Genotype = Graph[Nothing, Nothing]
  type Phenotype = Image

  override val populationSize: Int = 500
  override val tournamentSize = 5
  override def mutationCount(g: Genotype) = random.nextInt(1, 4)
  // override def mutationCount(g: Genotype) = 1

  val seed = 0
  override val parallel: Boolean = true

  val inputs = List.tabulate(128)(x => v(x))
  val outputs = List.tabulate(1)(x => v(x + 128))
  val baseGenotype = Graph(vertices = (inputs ++ outputs).toSet)

  def byteToBoolArray(inBytes: Array[Byte]): Array[Boolean] = {
    val outBools = new Array[Boolean](inBytes.size * 8)
    for ((byte, index) <- inBytes zipWithIndex) {
      for (bit <- 0 until 8) {
        outBools(index * 8 + (7 - bit)) = ((byte >> bit) & 1) == 1
      }
    }
    outBools
  }

  def calcMd5Sum(inBytes: Array[Byte]): Array[Byte] = {
    val md = MessageDigest.getInstance("MD5")
    md.update(inBytes)
    md.digest()
  }

  def genExamples(n: Int) = {
    val random = new scala.util.Random(seed)
    List.fill(n) {
      val in = new Array[Byte](16)
      random.nextBytes(in)
      val out = calcMd5Sum(in)
      byteToBoolArray(in) -> byteToBoolArray(out).take(1)
    }
  }

  var nextDraw = Duration.Zero.fromNow
  override def afterFitness(_population: Population, fitness: (Genotype) => Double, generation: Int) {

    val population = _population.sortBy(fitness).reverse
    if (nextDraw.timeLeft <= Duration.Zero) {
      nextDraw = 3 seconds fromNow
      Future {
        val best = population.head
        File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best, inputs, outputs))
      }
    }

    val best = fitness(population.head)
    if (best >= examples.size * outputs.size) {
      exampleCount *= 2
      examples = genExamples(exampleCount)
    }
  }

  var exampleCount = 1
  var examples = genExamples(exampleCount)
  // val examples = (
  //   // Array(
  //   Array(false, false) -> Array(false) ::
  //   Array(false, true) -> Array(true) ::
  //   Array(true, false) -> Array(true) ::
  //   Array(true, true) -> Array(false) ::
  //   // Array(true) -> Array(false) ::
  //   // Array(false) -> Array(true) ::
  //   Nil
  // )

  def calculateFitness(g: Genotype, prefix: String): Double = {
    val circuit = Circuit(inputs, outputs, g)
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

  override def stats(best: Genotype) = s"/ ${(examples.size * outputs.size)}[$exampleCount], gates: ${best.vertices.size - inputs.size}, e: ${best.edges.size}"
}
