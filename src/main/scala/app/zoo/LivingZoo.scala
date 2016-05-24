package aiolia.app.zoo

import aiolia.grammar.Grammar
import aiolia.util._
import aiolia.graph.DSL._

class LivingZoo(config: ZooConfig) {
  import config._

  def baseGenotype: Grammar[Double,Double] = Grammar.inOut(Creature.inputs, Creature.outputs, initVertexData)
  def newCreature(position: Vec2) = Creature(mutate(baseGenotype), initialEnergy, position)
  def randomPosition(world: World): Vec2 = Vec2(random.r.nextInt(world.dimensions.x), random.r.nextInt(world.dimensions.y))

  def live() {
    val world = new World(worldDimensions)
    println("Initial world:")
    println(world)
    while (true) {
      println("#" * 10)

      assurePopulation(world)
      assureFood(world)
      tick(world)

      writeDOTToFile(world)
      println(world)
      Thread.sleep(200)
    }
  }

  def assurePopulation(world: World) {
    if (world.creatures.size >= minimumPopulation) return

    val position = randomPosition(world)
    val place = world(position)
    if (place.creature.isEmpty)
      world.place(newCreature(position))
  }

  def assureFood(world: World) {
    val rand = random.r.nextDouble
    if (rand <= foodProbability) {
      val position = randomPosition(world)
      world.place(Apple, position)
    }
  }

  def tick(world: World) {
    world.creatures.foreach { creature =>
      val currentPlace = world(creature.position)
      val sensors = world.sensors(creature.position)

      currentPlace.foods.headOption.foreach { food =>
        creature.eat(food)
        world.remove(food, currentPlace.position)
      }

      creature.think(sensors, thinkEffort)

      if (creature.canReplicate) {
        val birthplace = world.neighbourPlaces(creature.position).find(_.creature.isEmpty)
        birthplace.foreach { place =>
          val passedOnEnergy = creature.replicate()
          val child = Creature(mutate(creature.genotype), passedOnEnergy, place.position)
          world.place(child)
        }
      }

      if (creature.isAlive) {
        val clamped = world.clamp(creature.position + creature.direction)
        val target = world(clamped)
        if (target.creature.isEmpty) {
          creature.walk(creature.position distance target.position, walkEffort)
          world.move(creature, target.position)
        }
      } else {
        world.remove(creature)
      }
    }
  }

  def writeDOTToFile(world: World) {
    if (world.creatures.isEmpty) return

    val oldest = world.creatures.maxBy(_.age)
    File.write("oldest.dot", DOTExport.toDOT(oldest.genotype.expand, Creature.inputs, Creature.outputs))
  }

  def mutate(genotype: Grammar[Double,Double]): Grammar[Double,Double] = {
    var curr = genotype
    for (_ <- 0 to mutationCount) {
      val mut = random.select(mutationOperators)
      curr = mut(curr).getOrElse(curr)
    }

    curr
  }
}
