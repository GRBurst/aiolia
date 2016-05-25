package aiolia.app.zoo

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import world._
import aiolia.grammar.Grammar
import aiolia.util._
import aiolia.graph.DSL._

class LivingZoo(config: ZooConfig) {
  import config._

  def baseGenotype: Grammar[Double, Double] = Grammar.inOut(Brain.inputs, Brain.outputs, initVertexData)
  def newCreature(pos: Vec2) = Creature(mutate(baseGenotype), initialEnergy, pos)
  def randomPosition(world: World): Vec2 = Vec2(random.r.nextInt(world.dimensions.x), random.r.nextInt(world.dimensions.y))
  def randomPositionAround(world: World, pos: Vec2, radius: Double = 1): Vec2 = pos + Vec2((random.r.nextGaussian * radius).round.toInt, (random.r.nextGaussian * radius).round.toInt)
  def live() {
    val world = new World(worldDimensions)
    println("Initial world:")
    println(world)
    while (true) {
      assurePopulation(world)
      assureFood(world)
      tick(world)

      writeDOTToFile(world)
      println(world)
      println("#" * world.dimensions.x * 5)
      Thread.sleep(200)
    }
  }

  def assurePopulation(world: World) {
    if (world.creatures.size >= minimumPopulation) return

    val pos = randomPosition(world)
    if (world(pos).isEmpty)
      world.add(newCreature(pos))
  }

  def assureFood(world: World) {
    if (random.r.nextDouble <= foodProbability) {
      val pos = randomPositionAround(world, world.dimensions / 2, radius = world.dimensions.x / 8)
      if (world.isInside(pos) && world(pos).isEmpty)
        world.add(new Apple(pos))
    }
  }

  def tick(world: World) {
    world.creatures.foreach { creature =>
      val sensors = world.sensors(creature.pos, creature.brain.rotation, 4)
      creature.think(sensors, thinkEffort)

      if (creature.canReplicate) {
        val birthPos = world.emptyNeighbourPositions(creature.pos).headOption
        birthPos.foreach { pos =>
          val passedOnEnergy = creature.replicate()
          val child = Creature(mutate(creature.genotype), passedOnEnergy, pos)
          world.add(child)
        }
      }
      if (creature.isAlive) {
        if (!creature.direction.isZero) {
          val newPos = world.clamp(creature.pos + creature.direction * creature.brain.speed)
          creature.walk(creature.direction.length * creature.brain.speed.abs, walkEffort)
          if (newPos != creature.pos) {
            world.lookup(newPos) match {
              case None =>
                world.move(creature, newPos)
              case Some(food: Food) =>
                creature.eat(food)
                world.remove(food)
                world.move(creature, newPos)
              case Some(other: Creature) =>
                if (creature.brain.agression >= 0 || other.brain.agression >= 0) {
                  if (creature.energy > other.energy) {
                    creature.energy -= other.energy
                    world.remove(other)
                    world.move(creature, newPos)
                  }
                  else {
                    other.energy -= creature.energy
                    world.remove(creature)
                  }
                }
              // TODO: fight? push?
            }
          }
        }
      }
      else {
        world.remove(creature)
        world.add(new Corpse(creature))
      }
    }
  }

  var nextDraw = Duration.Zero.fromNow

  def writeDOTToFile(world: World) {
    if (world.creatures.isEmpty) return

    if (nextDraw.timeLeft <= Duration.Zero) {
      nextDraw = 5 seconds fromNow
      Future {
        val oldest = world.creatures.maxBy(_.age)
        File.write("/tmp/oldest.dot", DOTExport.toDOT(oldest.genotype.expand, Brain.inputs, Brain.outputs))
      }
    }
  }

  def mutate(genotype: Grammar[Double, Double]): Grammar[Double, Double] = {
    var curr = genotype
    for (_ <- 0 to mutationCount) {
      val mut = random.select(mutationOperators)
      curr = mut(curr).getOrElse(curr)
    }

    curr
  }
}