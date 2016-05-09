package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers._
import aiolia.graph.dsl._
import aiolia.export.DOTExport

object ImageCompression extends App {
  val target = Image.read("apple.jpg")
  var resizeLevel = 1
  var resizedTarget = target.resized(resizeLevel)
  val seed = 0
  val mutationStrength = 3
  val populationSize = 100

  val random = Random(seed)
  val ANNAxiom = Graph(V(0, 1, 2, 3, 4), nonTerminals = List(nt(1, (0, 1, 2, 3, 4))))
  val ANNGrammar = Grammar(ANNAxiom, Map(1 -> Graph(V(0, 1, 2, 3, 4), connectors = C(0, 1, 2, 3, 4))))
  val mut = new FeedForwardNetworkMutation(seed, feedForwardInputs = VL(0, 1), feedForwardOutputs = VL(2, 3, 4))

  println("preparing initial population...")
  var population = List.fill(populationSize)(Mutation.mutate(ANNGrammar, mut, mutationStrength))
  for (gen <- 0 until 10000) {
    println("calculating fitness...")
    val fitness = population.par.map(i => (i -> calculateFitness(i))).toMap.seq
    val best = population.maxBy(fitness)

    generateImage(best).write(s"current.png")
    println(s"\ngeneration: $gen, picture width: $resizeLevel, fitness: ${fitness(best)}")
    File.write("currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs = VL(0, 1), feedForwardOutputs = VL(2, 3, 4)))

    println("preparing next population...")

    if (fitness(best) >= -0.25) {
      resizeLevel += 1
      println(s"resize to width: $resizeLevel")
      resizedTarget = target.resized(resizeLevel)
      resizedTarget.write("currentresized.png")
    }

    population = best :: (0 until (populationSize - 1)).map(_ => tournamentSelection(population, fitness, 3)).par.map(i => Mutation.mutate(i, mut, mutationStrength)).toList
  }

  def tournamentSelection[I](population: Iterable[I], fitness: Map[I, Double], n: Int): I = {
    random.select(population, n).maxBy(fitness)
  }

  def generateImage(grammar: Grammar[Double, Double], w: Int = resizedTarget.w, h: Int = resizedTarget.h): Image = {
    val network = FeedForwardNeuralNetwork(VL(0, 1), VL(2, 3, 4), grammar.expand)
    val image = Image.create(w, h)
    image.fill{ (x, y) =>
      val col = network.compute(Array(x, y))
      (col(0), col(1), col(2))
    }
  }

  def calculateFitness(grammar: Grammar[Double, Double]): Double = {
    -generateImage(grammar).similarity(resizedTarget)
  }
}
