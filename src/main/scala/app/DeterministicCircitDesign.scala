package aiolia.app

import aiolia.circuit._
import aiolia.util.{DOTExport, _}
import aiolia.graph.DSL._
import aiolia.graph._

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import java.security.MessageDigest

object DeterministicCircuitDesign extends App {
  type NANDLogic = Graph[Nothing, Nothing]
  type Examples = Seq[(Array[Boolean], Array[Boolean])]

  val inputs = List.tabulate(128)(x => v(x))
  val outputs = List.tabulate(1)(x => v(x + 128))

  var current = Graph(vertices = (inputs ++ outputs).toSet)
  var exampleCount = 1
  var examples = genExamples(exampleCount)
  while (correctCircuit(current, examples)) {
    println(exampleCount)
    exampleCount += 1
    examples = genExamples(exampleCount)
  }

  // override def genotypeCleanup(g: Genotype) = Simplify.simplify(inputs, outputs, g)

  def separatesExamples(separators: List[Vertex], inputs: List[Vertex], outputs: List[Vertex], network: NANDLogic, examplesA: Examples, examplesB: Examples) = {
    val c = Circuit(inputs, outputs, network)
    val intermediateResultsA: Seq[Array[Boolean]] = examplesA.map { case (eIn, _) => c.computeIntermediateResults(eIn) }
    val intermediateResultsB: Seq[Array[Boolean]] = examplesB.map { case (eIn, _) => c.computeIntermediateResults(eIn) }
    val bitsA: Set[Boolean] = intermediateResultsA.map { r => !separators.map(s => r(s.label)).reduce(_ && _) }.toSet
    val bitsB: Set[Boolean] = intermediateResultsB.map { r => !separators.map(s => r(s.label)).reduce(_ && _) }.toSet
    bitsA.size == 1 && bitsB.size == 1 && bitsA != bitsB
  }

  assert(separatesExamples(VL(0), VL(0, 1), VL(2), Graph(V(0, 1, 2)), List(
    Array(false, false) -> Array(false),
    Array(false, true) -> Array(false),
    Array(true, false) -> Array(false)
  ), List(
    Array(true, true) -> Array(true)
  )))

  def correctCircuit(current: NANDLogic, examples: Examples): Boolean = {
    val c = Circuit(inputs, outputs, current)
    examples.forall { case (eIn, eOut) => c.compute(eIn) == eOut }
  }

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

  val seed = 0
  def genExamples(n: Int): Examples = {
    val random = new scala.util.Random(seed)
    List.fill(n) {
      val in = new Array[Byte](16)
      random.nextBytes(in)
      val out = calcMd5Sum(in)
      byteToBoolArray(in) -> byteToBoolArray(out).take(1)
    }
  }
}
