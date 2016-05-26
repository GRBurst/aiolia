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
  def randomPosition(world: World): Vec2 = Vec2(random.r.nextInt(world.gen.dimensions.x), random.r.nextInt(world.gen.dimensions.y))
  //TODO: Clamp to world
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
      Thread.sleep(100)
    }
  }


  //TODO: Why is position set twice (update + newCreature)
  def assurePopulation(world: World) {
    if (world.gen.creatures.size >= minimumPopulation) return

    val pos = randomPosition(world)
    if (world.gen(pos).isEmpty)
      world.nextGen(pos) = newCreature(pos)
  }

  //TODO: Why is position set twice (update + Apple)
  def assureFood(world: World) {
    if (random.r.nextDouble <= foodProbability) {
      val pos = randomPositionAround(world, world.gen.dimensions / 2, radius = 2.5)
      if (world.gen.isInside(pos) && world.gen(pos).isEmpty)
        world.nextGen(pos) = new Apple(pos)

    }
  }

  def tick(world: World) {
    world.gen.creatures.foreach { creature =>
      val sensors = world.gen.sensors(creature.pos, creature.brain.rotation, 3) //TODO: create a global setting (mutate this in the future)
      creature.think(sensors, thinkEffort)
    }

    // TODO:
    world.gen.positions.foreach { pos =>
      val neighbourPositions = world.gen.neighbourPositions(pos)
      val creaturesGoingHere = neighbourPositions.map(pos => (pos, world.gen(pos))).collect{
        case (p, Some(c: Creature)) if (p + c.direction * c.brain.speed) == pos => c
      }

      world.gen(pos) match {
        case Some(thing) => world.nextGen(pos) = Some(thing)
        case None        => 
      }

    }

    world.swapBuffers()

    // world.creatures.foreach { creature =>
    //   if (creature.canReplicate) {
    //     val birthPos = world.emptyNeighbourPositions(creature.pos).headOption
    //     birthPos.foreach { pos =>
    //       val passedOnEnergy = creature.replicate()
    //       val child = Creature(mutate(creature.genotype), passedOnEnergy, pos)
    //       world.add(child)
    //     }
    //   }
    //   if (creature.isAlive) {
    //     if (!creature.direction.isZero) {
    //       val newPos = world.clamp(creature.pos + creature.direction * creature.brain.speed)
    //       creature.walk(creature.direction.length * creature.brain.speed.abs, walkEffort)
    //       if (newPos != creature.pos) {
    //         creature.walkedDistance += 1
    //         world.lookup(newPos) match {
    //           case None =>
    //             world.move(creature, newPos)
    //           case Some(food: Food) =>
    //             creature.eat(food)
    //             world.remove(food)
    //             world.move(creature, newPos)
    //           case Some(other: Creature) =>
    //             if (creature.brain.agression >= 0) {
    //               if (other.brain.agression >= 0) { // fight to death
    //                 if (creature.energy > other.energy) { // creature is stronger
    //                   creature.energy -= other.energy
    //                   world.remove(other)
    //                   world.move(creature, newPos)
    //                 }
    //                 else { // creature is weaker
    //                   other.energy -= creature.energy
    //                   world.remove(creature)
    //                 }
    //               }
    //               else { // other does not defend itself and gets eaten
    //                 creature.energy += other.energy * 0.5
    //                 world.remove(other)
    //                 world.move(creature, newPos)
    //               }
    //             }
    //         }
    //       }
    //     }
    //   }
    //   else {
    //     world.remove(creature)
    //     world.add(new Corpse(creature))
    //   }
    // }
  }

  var nextDraw = Duration.Zero.fromNow

  def writeDOTToFile(world: World) {
    if (world.gen.creatures.isEmpty) return

    if (nextDraw.timeLeft <= Duration.Zero) {
      nextDraw = 5 seconds fromNow
      Future {
        val oldest = world.gen.creatures.maxBy(_.age)
        File.write("/tmp/oldest.dot", DOTExport.toDOT(oldest.genotype.expand, Brain.inputs, Brain.outputs, Brain.labelNames))
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
