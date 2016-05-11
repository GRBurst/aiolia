package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers._
import aiolia.graph.dsl._
import aiolia.export.DOTExport

object ImageCompression extends App {
  for (seed <- 0 to 0) {
    val originalTarget = Image.read("apple.jpg")
    var resizeLevel = 16
    var target = originalTarget.resized(resizeLevel)
    target.write("/tmp/currentresized.png")
    // val seed = 1
    def mutationStrength(elements: Int): Int = 5 + (elements * 0.01).ceil.toInt //(Math.log(elements) / Math.log(2)).ceil.toInt
    val populationSize = 200
    val similarityResizeThreshold = 0.2
    val compilePixelThreshold = 200000
    val tournamentSize = 4

    val random = Random(seed)
    val ANNAxiom = Graph(V(0, 1, 2, 3, 4), nonTerminals = List(nt(1, (0, 1, 2, 3, 4))))
    val ANNGrammar = Grammar(ANNAxiom, Map(1 -> Graph(V(0, 1, 2, 3, 4), connectors = C(0, 1, 2, 3, 4))))
    val mut = new FeedForwardNetworkMutation(seed, feedForwardInputs = VL(0, 1), feedForwardOutputs = VL(2, 3, 4))

    def mutatePopulation(population: Population) = {
      population.map(g => Mutation.mutate(g, mut, mutationStrength(g.numElements)))
    }

    // println("preparing initial population...")
    type Genotype = Grammar[Double, Double]
    type Population = List[Genotype]
    var population: Population = mutatePopulation(List.fill(populationSize)(ANNGrammar))

    for (gen <- 0 until 10000000) {
      val fitness = calculateAllFitnesses(population)
      val best = population.maxBy(fitness)

      print("\rpreviews...     ")
      generateImage(best).write(s"/tmp/current.png")
      File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs = VL(0, 1), feedForwardOutputs = VL(2, 3, 4)))
      File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))

      val bestSimilarity = pictureSimilarity(best, "")
      checkResizeTarget(bestSimilarity)

      print("\rselection...            ")
      population = selectNextPopulation(population, fitness)

      print("\rmutation...             ")
      population = population.head :: mutatePopulation(population.tail) // don't mutate best

      println(s"\rgen: $gen, width: $resizeLevel (${target.pixels}px), sim: ${"%6.4f" format bestSimilarity}, fit: ${"%6.4f" format fitness(best)}, el: ${best.numElements}, comp: ${"%4.2f" format (best.compressionRatio)}, rules: ${best.productions.size}, components: ${best.expand.connectedComponents.size}")
    }

    def checkResizeTarget(similarity: Double) {
      if (similarity < similarityResizeThreshold) {
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

      if (prefix.nonEmpty) print(s"$prefix fitness...      ")
      image.fill{ (x, y) =>
        val Array(r, g, b) = network.compute(Array(x, y))
        (r, g, b)
      }
    }

    def pictureSimilarity(grammar: Genotype, prefix: String) = {
      val im = generateImage(grammar, prefix = prefix)
      if (prefix.nonEmpty) print(s"$prefix similarity...      ")
      im.similarity(target)
    }

    def calculateFitness(grammar: Genotype, prefix: String): Double = {
      var sum = 0.0
      sum -= Math.pow(1 + pictureSimilarity(grammar, prefix), 3) * 20
      sum -= Math.log(grammar.numElements.toDouble) * 0.001
      sum += Math.log(1 + Math.log(1 + grammar.compressionRatio)) * 0.1
      sum -= grammar.expand.connectedComponents.size
      sum
    }
  }

}
