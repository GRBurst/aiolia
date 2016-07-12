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
  ga.runFor(1000)
}

case class CircuitDesignConfig() extends Config[Graph[Nothing, Nothing]] with CircuitConfig {
  config =>
  type Genotype = Graph[Nothing, Nothing]
  type Phenotype = Image

  override val populationSize: Int = 200
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
    examples.count {
      case (in, shouldOut) =>
        val result = circuit.compute(in)
        // println(g, in.mkString(","), result.mkString(", "))
        result sameElements shouldOut
    } + 1.0 / g.numElements
  }

  override def stats(best: Genotype) = s", el: ${best.numElements}, g: ${best}"
}
