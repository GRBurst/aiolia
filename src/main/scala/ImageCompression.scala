package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers._
import aiolia.graph.dsl._
import aiolia.export.DOTExport

object ImageCompression extends App {
  for (seed <- 0 to 0) {
    val random = Random(seed)
    val originalTarget = Image.read("apple.jpg")
    var resizeLevel = 64
    var target = originalTarget.resized(resizeLevel)
    target.write("/tmp/currentresized.png")
    // val seed = 1
    def mutationStrength(elements: Int): Int = 4
    val populationSize = 100
    val tournamentSize = 4
    val distanceResizeThreshold = 0.18
    val compilePixelThreshold = 200000

    val feedForwardInputs = VL(0, 1)
    val feedForwardOutputs = VL(2, 3, 4)
    val ANNAxiom = Graph(Set.empty ++ feedForwardInputs ++ feedForwardOutputs, nonTerminals = List(NonTerminal(1, feedForwardInputs ++ feedForwardOutputs)))
    val ANNGrammar = Grammar(ANNAxiom, Map(1 -> Graph(ANNAxiom.vertices, connectors = feedForwardInputs ++ feedForwardOutputs)))
    val mut = new FeedForwardNetworkMutation(seed, feedForwardInputs, feedForwardOutputs)

    def mutatePopulation(population: Population): Population = {
      population.par.map(g => Mutation.mutate(g, mut, mutationStrength(g.numElements))).seq.toList
    }

    // println("preparing initial population...")
    type Genotype = Grammar[Double, Double]
    type Population = List[Genotype]
    var population: Population = mutatePopulation(List.fill(populationSize)(ANNGrammar))

    for (gen <- 0 until 10000) {
      val fitness = calculateAllFitnesses(population)
      val best = population.maxBy(fitness)

      print("\rpreviews...     ")
      generateImage(best).write(s"/tmp/current.png")
      File.write("/tmp/currentgraph.dot", DOTExport.toDOT(best.expand, feedForwardInputs, feedForwardOutputs))
      File.write("/tmp/currentgrammar.dot", DOTExport.toDOT(best))

      val bestDistance = pictureDistance(best, "")
      // checkResizeTarget(bestDistance)

      print("\rselection...            ")
      population = selectNextPopulation(population, fitness)

      print("\rmutation...             ")
      population = population.head :: mutatePopulation(population.tail) // don't mutate best

      println(s"\rgen: $gen, width: $resizeLevel (${target.pixels}px), sim: ${"%6.4f" format bestDistance}, fit: ${"%6.4f" format fitness(best)}, el: ${best.numElements}, comp: ${"%4.2f" format (best.compressionRatio)}, rules: ${best.productions.size}, components: ${best.expand.connectedComponents.size}")
    }

    def checkResizeTarget(distance: Double) {
      if (distance < distanceResizeThreshold) {
        resizeLevel *= 2
        println(s"\rresize to width: $resizeLevel      ")
        target = originalTarget.resized(resizeLevel)
        target.write("/tmp/currentresized.png")
      }
    }

    def selectNextPopulation(population: Population, fitness: Map[Genotype, Double]): Population = {
      val best = population.maxBy(fitness)
      best :: List.fill(populationSize - 1)(tournamentSelection(population, fitness, tournamentSize))
      // List.fill(populationSize)(best)
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
      val network = FeedForwardNeuralNetwork(feedForwardInputs, feedForwardOutputs, grammar.expand)
      val image = Image.create(w, h)

      if (image.pixels >= compilePixelThreshold) {
        if (prefix.nonEmpty) print(s"$prefix compiling...      ")
        network.compile()
      }

      if (prefix.nonEmpty) print(s"$prefix fitness...      ")
      image fill network.compute
    }

    def pictureDistance(grammar: Genotype, prefix: String) = {
      val im = generateImage(grammar, prefix = prefix)
      if (prefix.nonEmpty) print(s"$prefix distance...      ")
      im.distance(target)
    }

    def calculateFitness(grammar: Genotype, prefix: String): Double = {
      var sum = 0.0
      sum -= pictureDistance(grammar, prefix)
      sum -= grammar.numElements.toDouble * 0.000005
      // sum += Math.log(1 + Math.log(1 + grammar.compressionRatio)) * 0.1
      // sum -= (grammar.expand.vertices -- V(2)).count(grammar.expand.outDegree(_) > 0) * 0.001
      sum -= grammar.expand.connectedComponents.size * 0.01
      sum
    }
  }

}
