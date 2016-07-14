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

object CircuitDesign extends App {
  val ga = GeneticAlgorithm(CircuitDesignConfig())
  ga.runFor(100)
}

case class CircuitDesignConfig() extends Config[Graph[Nothing, Nothing]] with CircuitConfig {
  config =>
  type Genotype = Graph[Nothing, Nothing]
  type Phenotype = Image

  override val populationSize: Int = 50
  override val tournamentSize = 3
  override def mutationCount(g: Genotype) = 2

  val seed = 0
  override val parallel: Boolean = false

  val inputs = VL(0, 1)
  val outputs = VL(2)
  val baseGenotype = Graph(vertices = (inputs ++ outputs).toSet)

  val examples = (
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
    var score: Int = 0
    for ((in, shouldOut) <- examples) {
      val computedOut = circuit.compute(in)
      for ((computedOutBit, shouldOutBit) <- computedOut zip shouldOut) {
        score += (if (computedOutBit == shouldOutBit) 1 else 0)
      }
    }
    val circuitSizeScore = 1.0 / (g.vertices.size)
    score + circuitSizeScore
  }

  override def stats(best: Genotype) = s", gates: ${best.vertices.size - inputs.size}, e: ${best.edges.size}, g: ${best}"
}
