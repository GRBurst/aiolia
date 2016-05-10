package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers._
import aiolia.graph.dsl._
import aiolia.export.DOTExport

object ImageCompression extends App {
  for (seed <- 0 to 0) {
    val originalTarget = Image.read("apple.jpg")
    var resizeLevel = 1
    var target = originalTarget.resized(resizeLevel)
    target.write("/tmp/currentresized.png")
    // val seed = 1
    def mutationStrength(elements: Int): Int = (Math.log(elements) / Math.log(10)).ceil.toInt
    val populationSize = 300
    val fitnessResizeThreshold = -0.1
    val compilePixelThreshold = 200000
    val tournamentSize = 3

    val random = Random(seed)
    val ANNAxiom = Graph(V(0, 1, 2, 3, 4), nonTerminals = List(nt(1, (0, 1, 2, 3, 4))))
    val ANNGrammar = Grammar(ANNAxiom, Map(1 -> Graph(V(0, 1, 2, 3, 4), connectors = C(0, 1, 2, 3, 4))))
    val mut = new FeedForwardNetworkMutation(seed, feedForwardInputs = VL(0, 1), feedForwardOutputs = VL(2, 3, 4))

    // println("preparing initial population...")
    type Genotype = Grammar[Double, Double]
    type Population = List[Genotype]
    var population = List.fill(populationSize)(Mutation.mutate(ANNGrammar, mut, 1))
    for (gen <- 0 until 100000) {
      val fitness = calculateAllFitnesses(population)
      val best = population.maxBy(fitness)

      print("\rgenerating previews...     ")
      generateImage(best).write(s"/tmp/current.png")
      File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs = VL(0, 1), feedForwardOutputs = VL(2, 3, 4)))
      File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))

      checkResizeTarget(fitness, best)

      print("\rselection...            ")
      population = selectNextPopulation(population, fitness)
      print("\rmutation...             ")
      population = mutatePopulation(population)

      println(s"\rgen: $gen, width: $resizeLevel (${target.pixels}px), fit: ${fitness(best)}, elements: ${best.numElements}, compression: ${"%4.2f" format (1 - best.compressionRatio)}, isolatedV: ${best.expand.isolatedVertices.size}")
    }

    def checkResizeTarget(fitness: Map[Genotype, Double], best: Genotype) {
      if (fitness(best) >= fitnessResizeThreshold) {
        resizeLevel *= 2
        println(s"\rresize to width: $resizeLevel      ")
        target = originalTarget.resized(resizeLevel)
        target.write("/tmp/currentresized.png")
      }
    }

    def selectNextPopulation(population: Population, fitness: Map[Genotype, Double]): Population = {
      val best = population.maxBy(fitness)
      best :: List.fill(populationSize - 1)(tournamentSelection(population, fitness, tournamentSize))
    }

    def mutatePopulation(population: Population) = {
      population.map(g => Mutation.mutate(g, mut, mutationStrength(g.numElements)))
    }

    def calculateAllFitnesses(population: Population): Map[Genotype, Double] = {
      var done = 0
      population.par.map{ g =>
        val prefix = s"\r${done} / $populationSize:"
        val f = calculateFitness(g, prefix)
        done += 1
        (g -> f)
      }.toMap.seq
    }

    def tournamentSelection[I](population: Iterable[I], fitness: Map[I, Double], n: Int): I = {
      random.select(population, n).maxBy(fitness)
    }

    def generateImage(grammar: Genotype, w: Int = target.w, h: Int = target.h, prefix: String = ""): Image = {
      val network = FeedForwardNeuralNetwork(VL(0, 1), VL(2, 3, 4), grammar.expand)
      val image = Image.create(w, h)

      if (image.pixels >= compilePixelThreshold) {
        if (prefix.nonEmpty) print(s"$prefix compiling...      ")
        network.compile()
      }

      if (prefix.nonEmpty) print(s"$prefix evaluating...      ")
      image.fill{ (x, y) =>
        val Array(r, g, b) = network.compute(Array(x, y))
        (r, g, b)
      }
    }

    def calculateFitness(grammar: Genotype, prefix: String): Double = {
      val r = -generateImage(grammar, prefix = prefix).similarity(target)
      r
    }
  }

}
