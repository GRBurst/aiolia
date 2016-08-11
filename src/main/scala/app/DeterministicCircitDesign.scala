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
  type TruthTable = Seq[(Array[Boolean], Array[Boolean])]

  val inputs = List.tabulate(128)(x => v(x))
  val outputs = List.tabulate(1)(x => v(x + 128))

  val combinators: List[Circuit] = List(
    Circuit(VL(0), VL(1), Graph(V(0, 1), E(0 -> 1))),
    Circuit(VL(0), VL(1), Graph(V(0, 1, 2), E(0 -> 2, 2 -> 1))),
    Circuit(VL(0, 1), VL(2), Graph(V(0, 1, 2, 3), E(0 -> 3, 1 -> 3, 3 -> 2))),
    Circuit(VL(0, 1), VL(2), Graph(V(0, 1, 2, 3, 4), E(0 -> 3, 1 -> 3, 3 -> 4, 4 -> 2)))
  )
  // print("compiling combinator circuits")
  // combinators.foreach { c => c.compile(); print(".") }
  // println("")
  val maxCombinationSize = combinators.map(_.in.size).max
  val minCombinationSize = combinators.map(_.in.size).min

  var currentCircuit = Circuit(inputs, outputs, Graph(vertices = (inputs ++ outputs).toSet))
  var exampleCount = 1
  var examples = genExamples(exampleCount)
  for (_ <- 0 to 5) {
    while (correctCircuit(currentCircuit, examples)) {
      exampleCount += 1
      examples = genExamples(exampleCount)
    }
    println("exampleCount: " + exampleCount)

    // current circuit gives wrong result for last example
    // now try to find a circuit that separates the previous examples from the last one
    val (separatingVertices, separatorCircuit) = findSeparator(currentCircuit, examples.dropRight(1), examples.takeRight(1))
    // separatorCircuit results in a 0 for previous examples and 1 for last example
    // now insert separatorCircuit combined with an XOR of the previous output
    currentCircuit.graph.incomingEdges(currentCircuit.out.head) match {
      case incomingEdges if incomingEdges.isEmpty =>
        // default output was false, and in this case it was wrong,
        // so just insert the circuit and connect it to the output
        currentCircuit = currentCircuit.insertSubCircuit(separatingVertices, outputs, separatorCircuit)
        println(currentCircuit)
        assert(correctCircuit(currentCircuit, examples))
      case incomingEdges =>
        incomingEdges.head match {
          case e @ Edge(in, out) => {
            currentCircuit = currentCircuit.copy(graph = currentCircuit.graph - e)
            // insert xor (in, invertingSignal) -> out
            // insert separator with result being invertingSignal
            // currentCircuit = currentCircuit.insertSubCircuit(separatingVertices, outputs, separatorCircuit)
            // println(currentCircuit)
            // assert(correctCircuit(currentCircuit, examples))
          }
        }
    }
  }

  // override def genotypeCleanup(g: Genotype) = Simplify.simplify(inputs, outputs, g)

  def computeIntermediateResults(examples: TruthTable, c: Circuit): Seq[Array[Boolean]] = {
    examples.map { case (eIn, _) => c.computeIntermediateResults(eIn) }
  }
  def combinateSelectedResults(intermediateResults: Seq[Array[Boolean]], combination: Seq[Int], combinator: Circuit): Set[Boolean] = {
    intermediateResults.map { r => combinator.compute(combination.map(r).toArray)(0) }.toSet
  }
  def findSeparator(c: Circuit, examplesA: TruthTable, examplesB: TruthTable): (Seq[Vertex], Circuit) = {
    println("examplesA:")
    println(examplesA.map { case (eIn, eOut) => s"${eIn.mkString} -> ${eOut.mkString}" }.mkString("\n"))
    println("examplesB:")
    println(examplesB.map { case (eIn, eOut) => s"${eIn.mkString} -> ${eOut.mkString}" }.mkString("\n"))
    assert(examplesB.size >= 0)
    assert(examplesA.size == 0 || examplesA.head._1.size == examplesB.head._1.size)
    assert(examplesA.size == 0 || examplesA.head._2.size == examplesB.head._2.size && examplesA.head._2.size == 1)
    val intermediateResultsA: Seq[Array[Boolean]] = computeIntermediateResults(examplesA, c)
    val intermediateResultsB: Seq[Array[Boolean]] = computeIntermediateResults(examplesB, c)
    val indices = intermediateResultsB.head.indices
    val indexCombinations = (minCombinationSize to maxCombinationSize).toStream.flatMap(indices.combinations)
    // println(s"indices: $indices")
    // println(s"indexCombinations: $indexCombinations")
    // println(s"combinators: ${combinators.map { _.graph }}")
    //TODO: all permutations per combination
    for (combination <- indexCombinations; combinator <- combinators if combinator.in.size == combination.size) {
      // println(s"combination: $combination, combinator: ${combinator.graph}")
      val bitsA: Set[Boolean] = combinateSelectedResults(intermediateResultsA, combination, combinator)
      val bitsB: Set[Boolean] = combinateSelectedResults(intermediateResultsB, combination, combinator)
      // println(s"$bitsA vs $bitsB")
      if (bitsA.size <= 1 && bitsB.size == 1 && bitsA != bitsB && bitsB.head == true) {
        assert(bitsA.size == 0 || bitsA.head == false)
        return (combination map (Vertex(_)), combinator)
      }
    }
    throw new RuntimeException("no separator found")
  }

  def correctCircuit(c: Circuit, examples: TruthTable): Boolean = {
    examples.forall {
      case (eIn, eOut) =>
        val computed = c.compute(eIn)
        // println(s"${eIn.mkString(",")} -> ${eOut.mkString(",")}, computed: ${computed.mkString(",")}")
        computed sameElements eOut
    }
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
  def genExamples(n: Int): TruthTable = {
    val random = new scala.util.Random(seed)
    List.fill(n) {
      val in = new Array[Byte](16)
      random.nextBytes(in)
      val out = calcMd5Sum(in)
      byteToBoolArray(in) -> byteToBoolArray(out).take(1)
    }
  }
}
