package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers._
import aiolia.graph.dsl._
import aiolia.export.DOTExport

object ImageCompression extends App {
  for (seed <- 0 to 0) {
    val target = Image.read("DNA.jpg")
    var resizeLevel = 1
    var resizedTarget = target.resized(resizeLevel)
    resizedTarget.write("/tmp/currentresized.png")
    // val seed = 1
    val mutationStrength = 0.01
    val populationSize = 700

    val random = Random(seed)
    val ANNAxiom = Graph(V(0, 1, 2, 3, 4), nonTerminals = List(nt(1, (0, 1, 2, 3, 4))))
    val ANNGrammar = Grammar(ANNAxiom, Map(1 -> Graph(V(0, 1, 2, 3, 4), connectors = C(0, 1, 2, 3, 4))))
    val mut = new FeedForwardNetworkMutation(seed, feedForwardInputs = VL(0, 1), feedForwardOutputs = VL(2, 3, 4))

    // println("preparing initial population...")
    var population = List.fill(populationSize)(Mutation.mutate(ANNGrammar, mut, 1))
    for (gen <- 0 until 100000) {
      // println("calculating fitness...")
      val fitness = population.par.map(i => (i -> calculateFitness(i))).toMap.seq
      val best = population.maxBy(fitness)

      generateImage(best).write(s"/tmp/current.png")
      println(s"gen: $gen, pic width: $resizeLevel, fit: ${fitness(best)}, elements: ${best.numElements}")
      File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs = VL(0, 1), feedForwardOutputs = VL(2, 3, 4)))
      File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))

      // println("preparing next population...")

      if (fitness(best) >= -0.1) {
        resizeLevel *= 2
        println(s"resize to width: $resizeLevel")
        resizedTarget = target.resized(resizeLevel)
        resizedTarget.write("/tmp/currentresized.png")
      }

      population = best :: (0 until (populationSize - 1)).map(_ => tournamentSelection(population, fitness, 4)).map(i => Mutation.mutate(i, mut, (i.numElements * mutationStrength).ceil.toInt)).toList
    }
    def tournamentSelection[I](population: Iterable[I], fitness: Map[I, Double], n: Int): I = {
      random.select(population, n).maxBy(fitness)
    }

    def generateImage(grammar: Grammar[Double, Double], w: Int = resizedTarget.w, h: Int = resizedTarget.h): Image = {
      val network = FeedForwardNeuralNetwork(VL(0, 1), VL(2, 3, 4), grammar.expand)
      val image = Image.create(w, h)
      if(image.pixels < 1000)
        image.fill{ (x, y) =>
          val col = network.compute_recursive(Array(x, y))
          (col(0), col(1), col(2))
        }
      else
        image.fill{ (x, y) =>
          val col = network.compute(Array(x, y))
          (col(0), col(1), col(2))
        }
    }

    def calculateFitness(grammar: Grammar[Double, Double]): Double = {
      -generateImage(grammar).similarity(resizedTarget)
    }
  }

}
