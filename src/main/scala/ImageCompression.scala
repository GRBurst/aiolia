package aiolia

import aiolia.graph._
import aiolia.graph.types._
import aiolia.helpers._
import aiolia.graph.dsl._

object ImageCompression extends App {
  val target = Image.read("apple_small.jpg")
  val seed = 0
  val mutationStrength = 5
  val populationSize = 100

  val ANNAxiom = Graph(V(0, 1, 2, 3, 4), nonTerminals = List(nt(1, (0, 1, 2, 3, 4))))
  val ANNGrammar = Grammar(ANNAxiom, Map(1 -> Graph(V(0, 1, 2, 3, 4), connectors = C(0, 1, 2, 3, 4))))
  val mut = new FeedForwardNetworkMutation(seed)

  println("preparing initial population...")
  var population = List.fill(populationSize)(Mutation.mutate(ANNGrammar, mut, mutationStrength))
  for (gen <- 0 until 10000) {
    val parent = best(population)
    generateImage(parent).write(s"current.png")
    println(s"\ngeneration: $gen\n$parent\n${parent.expand}")

    println("preparing next population...")
    population = List.fill(populationSize)(Mutation.mutate(parent, mut, mutationStrength))
  }

  def generateImage(grammar: Grammar[Double, Double]): Image = {
    val network = FeedForwardNeuralNetwork(VL(0, 1), VL(2, 3, 4), grammar.expand)
    val image = Image.create(target.w, target.h)
    image.fill{ (x, y) =>
      val col = network.compute(List(x, y))
      (col(0), col(1), col(2))
    }
  }

  def fitness(grammar: Grammar[Double, Double]): Double = {
    -generateImage(grammar).similarity(target)
  }

  def best(population: List[Grammar[Double, Double]]): Grammar[Double, Double] = {
    population.par.maxBy(fitness)
  }
}
