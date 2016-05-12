package aiolia

import helpers.Random
import scala.concurrent.duration._

trait GeneticAlgorithmConfig[Genotype] {
  type Population = List[Genotype]

  val seed: Any
  val baseGenotype: Genotype
  def calculateFitness(p: Genotype, prefix: String): Double
  def mutate(g: Genotype, prefix: String): Genotype

  val populationSize: Int = 30
  val tournamentSize = 4

  def stats(g: Genotype): String = ""
  def afterFitness(population: Population) {}
}

object GeneticAlgorithm {
  def apply[Genotype](config: GeneticAlgorithmConfig[Genotype]) = new GeneticAlgorithm(config)

}

class GeneticAlgorithm[Genotype](config: GeneticAlgorithmConfig[Genotype]) {
  type Population = List[Genotype]
  import config._

  val random = Random(seed)

  def mutation(population: Population): Population = {
    val elite :: others = population
    var done = 0
    val mutatedOthers = others.par.map{ g =>
      val prefix = s"\r${done} / ${populationSize - 1}:"
      val mutated = mutate(g, prefix)
      done += 1
      mutated
    }.seq.toList
    elite :: mutatedOthers
  }
  def selection(population: Population, elite: Genotype, fitness: (Genotype) => Double): Population = {
    elite :: List.fill[Genotype](populationSize - 1)(tournamentSelection(population, fitness, tournamentSize))
    // List.fill(populationSize)(best)
  }

  def tournamentSelection(population: Population, fitness: (Genotype) => Double, n: Int): Genotype = {
    random.select(population, n).maxBy(fitness)
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

  var population: Population = mutation(List.fill(populationSize)(baseGenotype))

  var generation = 0
  def nextGeneration() {
    val fitness = calculateAllFitnesses(population)
    val best = population.maxBy(fitness)
    afterFitness(population)

    print("\rselection...            ")
    population = selection(population, best, fitness)

    print("\rmutation...             ")
    population = mutation(population)

    println(s"\rgen: $generation, fit: ${"%6.4f" format fitness(best)} ${stats(best)}")
    generation += 1
  }

  def runFor(generations: Int) {
    for (_ <- 0 until generations) nextGeneration()
  }

  def runFor(duration: Duration) {
    duration match {
      case duration: FiniteDuration =>
        val deadline = duration.fromNow
        while (deadline.timeLeft > Duration.Zero) nextGeneration()
      case _: Duration.Infinite =>
        while (true) nextGeneration()
    }
  }

  def runIntil(condition: (Genotype) => Boolean) {
    while (!condition(population.head)) nextGeneration()
  }
}
