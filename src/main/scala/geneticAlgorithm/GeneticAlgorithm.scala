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

  protected var lastLogSize = 0
  val prefix = ""
  val nested = false
  def log(msg: String) { print(s"\r$prefix$msg".padTo(lastLogSize + 1, " ").mkString); lastLogSize = prefix.size + msg.size; System.out.flush() }
  def logln(msg: String) { if (nested) { log(msg); return }; println(s"\r$prefix$msg".padTo(lastLogSize + 1, " ").mkString); lastLogSize = 0 }

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

  var population: Population = List.fill(populationSize)(baseGenotype)
  var fitness: Map[Genotype, Double] = Map.empty
  var generation = 0

  def nextGeneration() {
    val genPrefix = s"[${"%4d" format generation}]"
    population = mutation(population, prefix = s"$genPrefix mutation: ")
    fitness = calculateAllFitnesses(population, s"$genPrefix fitness: ")
    val best = population.maxBy(fitness)

    afterFitness(population, fitness)

    logln(s"$genPrefix fit: ${"%6.4f" format fitness(best)} ${stats(best)}")

    population = selection(population, best, fitness)

    generation += 1
  }

  def calculateAllFitnesses(population: Population, prefix: String): Map[Genotype, Double] = {
    var current = 1
    def m(g: Genotype) = {
      val _prefix = s"$prefix$current / ${populationSize}: "
      if (!nested) log(_prefix)
      val f = calculateFitness(g, _prefix)
      current += 1
      g -> f
    }
    if (parallel) population.par.map(m).toMap.seq
    else population.map(m).toMap
  }

  def selection(population: Population, elite: Genotype, fitness: (Genotype) => Double): Population = {
    elite :: List.fill[Genotype](populationSize - 1)(tournamentSelection(population, fitness, tournamentSize))
  }

  def tournamentSelection(population: Population, fitness: (Genotype) => Double, n: Int): Genotype = {
    random.select(population, n min populationSize).maxBy(fitness)
  }

  def mutation(population: Population, prefix: String): Population = {
    val elite :: others = population
    var current = 1
    def m(g: Genotype) = {
      val _prefix = s"$prefix$current / ${populationSize - 1}: " // -1 because the first one is elite
      if (!nested) log(_prefix)
      val mutated = mutate(g, mutationCount(g), _prefix)
      current += 1
      mutated
    }
    if (parallel)
      elite :: others.par.map(m).seq.toList
    else
      elite :: others.map(m)
  }

  @tailrec final def mutate(genotype: Genotype, n: Int, prefix: String): Genotype = {
    assert(n >= 0)
    assert(genotypeInvariant(genotype), genotypeInvariantError)

    if (n == 0 || mutationOperators.isEmpty) afterMutationOp(genotype) //TODO: on every mutation op, or after all mutation ops?
    else {
      val operator = random.select(mutationOperators)
      // log(s"${prefix}mutation $n: ${operator.getClass.getName}")
      val tries = new Iterator[Option[Genotype]] {
        def hasNext = true
        def next = {
          // log(s"${prefix}mutation $n: ${operator.getClass.getName}")
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
