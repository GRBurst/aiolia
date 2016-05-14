package aiolia

import helpers.Random
import scala.concurrent.duration._
import annotation.tailrec

trait MutationOp[G] extends Function1[G, Option[G]] {
  type Genotype = G
}

trait MutationOpConfig[G] {
  val random: Random
}

trait GeneticAlgorithmConfig[Genotype] extends MutationOpConfig[Genotype] {
  type Population = List[Genotype]

  val seed: Any
  val random = Random(seed)
  val baseGenotype: Genotype
  def calculateFitness(p: Genotype, prefix: String): Double

  // TODO: post processing of genotype, eg: clean up isolated vertices
  val mutationOperators: List[MutationOp[Genotype]]
  def genotypeInvariant(g: Genotype): Boolean = true
  def genotypeInvariantError: String = ""

  val populationSize: Int = 30
  val tournamentSize = 4
  val mutationCount = 4

  def stats(g: Genotype): String = ""
  def afterFitness(population: Population) {}
  def afterMutationOp(g: Genotype): Genotype = g
}

object GeneticAlgorithm {
  def apply[Genotype](config: GeneticAlgorithmConfig[Genotype]) = new GeneticAlgorithm[Genotype, GeneticAlgorithmConfig[Genotype]](config)
}

class GeneticAlgorithm[Genotype, Config <: GeneticAlgorithmConfig[Genotype]](config: Config) {
  type Population = List[Genotype]
  import config._

  def mutation(population: Population): Population = {
    val elite :: others = population
    var done = 0
    val mutatedOthers = others.par.map{ g =>
      val prefix = s"\r${done} / ${populationSize - 1}:"
      val mutated = mutate(g, mutationCount, prefix)
      done += 1
      mutated
    }.seq.toList
    elite :: mutatedOthers
  }

  @tailrec final def mutate(genotype: Genotype, n: Int, prefix: String): Genotype = {
    assert(n >= 0)
    assert(genotypeInvariant(genotype), genotypeInvariantError)

    if (n == 0) afterMutationOp(genotype) //TODO: on every mutation op, or after all mutation ops?
    else {
      val operator = random.select(mutationOperators)
      val tries = new Iterator[Option[Genotype]] {
        def hasNext = true
        def next = {
          // println(s"$prefix  mutation $n: ${operator.getClass.getName}")
          operator(genotype)
        }
      }

      // println(s"$prefix start trying...")
      val maxTries = 5
      val result = tries.take(maxTries).flatten.take(1).toList
      // println(s"$prefix done")

      result match {
        case Nil => mutate(genotype, n, prefix)
        case List(mutatedGenotype) =>
          assert(genotypeInvariant(mutatedGenotype)) //, s"\nbefore ${operator.getClass.getName}:\n$genotype\nexpanded: ${genotype.expand}\nafter ${operator.getClass.getName}: $genotypeInvariantError\n${mutatedGenotype}\nexpanded: ${mutatedGenotype.expand}") //TODO: move to invariantError
          mutate(mutatedGenotype, n - 1, prefix)
      }
    }
  }

  def selection(population: Population, elite: Genotype, fitness: (Genotype) => Double): Population = {
    elite :: List.fill[Genotype](populationSize - 1)(tournamentSelection(population, fitness, tournamentSize))
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

  def runUntil(condition: (Genotype) => Boolean) {
    while (!condition(population.head)) nextGeneration()
  }
}
