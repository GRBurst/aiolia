package aiolia.geneticAlgorithm

import aiolia.util.Random

import scala.annotation.tailrec
import scala.concurrent.duration._

trait MutationOp[G] extends ((G) => Option[G]) {
  type Genotype = G
}

trait MutationOpConfig[G] {
  val random: Random
}

trait Config[Genotype] extends MutationOpConfig[Genotype] {
  type Population = List[Genotype]

  val seed: Any
  val random = Random(seed)
  val parallel = true
  val log = true
  val baseGenotype: Genotype
  def calculateFitness(g: Genotype, prefix: String): Double

  // TODO: post processing of genotype, eg: clean up isolated vertices
  val mutationOperators: List[(Genotype) => Option[Genotype]]
  def genotypeInvariant(g: Genotype): Boolean = true
  def genotypeInvariantError: String = "Genotype invariant violated."

  val populationSize: Int = 30
  val tournamentSize = 4
  def mutationCount(g: Genotype) = 1

  def stats(g: Genotype): String = ""
  def afterFitness(population: Population, fitness: (Genotype) => Double) {}
  def afterMutationOp(g: Genotype): Genotype = g
}

object GeneticAlgorithm {
  def apply[Genotype](config: Config[Genotype]) = new GeneticAlgorithm[Genotype, Config[Genotype]](config)
}

class GeneticAlgorithm[Genotype, C <: Config[Genotype]](config: C) {
  type Population = List[Genotype]
  import config._

  assert(populationSize >= tournamentSize)

  def mutation(population: Population): Population = {
    val elite :: others = population
    var done = 0
    val mutatedOthers = if (parallel) others.par.map{ g =>
      val prefix = s"\r$done / ${populationSize - 1}:"
      val mutated = mutate(g, mutationCount(g), prefix)
      done += 1
      mutated
    }.seq.toList
    else others.map{ g =>
      val prefix = s"\r$done / ${populationSize - 1}:"
      val mutated = mutate(g, mutationCount(g), prefix)
      done += 1
      mutated
    }
    elite :: mutatedOthers
  }

  @tailrec final def mutate(genotype: Genotype, n: Int, prefix: String): Genotype = {
    assert(n >= 0)
    assert(genotypeInvariant(genotype), genotypeInvariantError)

    if (n == 0 || mutationOperators.isEmpty) afterMutationOp(genotype) //TODO: on every mutation op, or after all mutation ops?
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

      val nextGenotype = result match {
        case Nil => genotype
        case List(mutatedGenotype) =>
          assert(genotypeInvariant(mutatedGenotype), s"$genotypeInvariantError\nbefore ${operator.getClass.getName}:\n$genotype\nafter ${operator.getClass.getName}:\n${mutatedGenotype}")
          mutatedGenotype
      }
      mutate(nextGenotype, n - 1, prefix)
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
    if (parallel)
      population.par.map{ g =>
        val prefix = s"\r$done / $populationSize:"
        val f = calculateFitness(g, prefix)
        done += 1
        g -> f
      }.toMap.seq
    else population.map{ g =>
      val prefix = s"\r$done / $populationSize:"
      val f = calculateFitness(g, prefix)
      done += 1
      g -> f
    }.toMap
  }

  var population: Population = mutation(List.fill(populationSize)(baseGenotype))

  var generation = 0
  def nextGeneration() {
    val fitness = calculateAllFitnesses(population)
    val best = population.maxBy(fitness)
    afterFitness(population, fitness)

    if (log) print("\rselection...            ")
    population = selection(population, best, fitness)

    if (log) print("\rmutation...             ")
    population = mutation(population)

    if (log) println(s"\rgen: $generation, fit: ${"%6.4f" format fitness(best)} ${stats(best)}")
    generation += 1
  }

  def runFor(generations: Int): Genotype = {
    for (_ <- 0 until generations) nextGeneration()
    population.head
  }

  def runFor(duration: Duration): Genotype = {
    duration match {
      case duration: FiniteDuration =>
        val deadline = duration.fromNow
        while (deadline.timeLeft > Duration.Zero) nextGeneration()
      case _: Duration.Infinite =>
        while (true) nextGeneration()
    }
    population.head
  }

  def runUntil(condition: (Genotype) => Boolean): Genotype = {
    while (!condition(population.head)) nextGeneration()
    population.head
  }
}
