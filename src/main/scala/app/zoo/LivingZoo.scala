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
  def newCreature = Creature(mutate(baseGenotype, random.r.nextInt(mutationCount)), initialEnergy)
  def randomPosition(world: World): Vec2 = Vec2(random.r.nextInt(world.gen.dimensions.x), random.r.nextInt(world.gen.dimensions.y))
  //TODO: Clamp to world
  def randomPositionAround(world: World, pos: Vec2, radius: Double = 1): Vec2 = pos + Vec2((random.r.nextGaussian * radius).round.toInt, (random.r.nextGaussian * radius).round.toInt)
  def live() {
    val world = new World(worldDimensions)

    println("Initial world:")
    println(world)
    // while (true) {
    for (i <- 0 until 1000000) {
      assurePopulation(world)
      assureFood(world)
      tick(world)

      if (i > 0) {
        writeDOTToFile(world)
        println(world)
        println("#" * world.dimensions.x * 3)
        // Thread.sleep(100)
      }
    }
  }

  //TODO: Where to write to? Should be next Gen, think about architecture
  def assurePopulation(world: World) {
    if (world.gen.creatures.size >= minimumPopulation) return

    val pos = randomPosition(world)
    if (world.gen(pos).isEmpty) {
      // println(s"spawning creature at $pos")
      world.gen(pos) = newCreature
    }
  }

  def assureFood(world: World) {
    if (random.r.nextDouble <= foodProbability) {
      for (treePos <- Vec2(0, 0) to Vec2(2, 2)) {
        val pos = randomPositionAround(world, treePos * world.dimensions / 2, radius = 6)
        if (world.gen.isInside(pos) && world.gen(pos).isEmpty)
          world.gen(pos) = Apple
      }

      {
        val pos = randomPositionAround(world, world.dimensions / 2, radius = 1)
        if (world.gen.isInside(pos) && world.gen(pos).isEmpty)
          world.gen(pos) = Banana
      }
    }
  }

  def tick(world: World) {
    assert(world.gen.creatures.map(_._2).size == world.gen.creatures.map(_._2).toSet.size)
    // feed the creature brains with sensor data
    world.gen.creatures.foreach {
      case (pos, creature) =>
        val sensors = world.gen.sensors(pos, creature.direction, 4) //TODO: create a global setting (mutate this in the future)
        creature.think(sensors, thinkEffort)
    }

    // replication
    world.gen.creatures.filter{ case (pos, c) => c.wantsToReplicate }.foreach {
      case (pos, creature) =>
        val birthPos = world.gen.emptyNeighbourPositions(pos).headOption
        birthPos.foreach { pos =>
          val passedOnEnergy = creature.replicate()
          val child = Creature(mutate(creature.genotype, (creature.brain.mutationStrength * mutationCount).round.toInt), passedOnEnergy)
          world.gen(pos) = child
          // println(s"$pos spawned new child: ${child.hashCode.toString.take(4)}")
        }
    }

    // val affectedPositions = world.gen.creatures.flatMap{ case (pos, _) => world.gen.neighbourPositions(pos) + pos }.toSet
    // affectedPositions.foreach { pos =>
    world.gen.positions.foreach { pos =>
      val neighbourPositions = world.gen.neighbourPositions(pos)
      val creaturesWanttoGoHere = neighbourPositions.map(pos => (pos, world.gen(pos))).collect{
        case (p, Some(c: Creature)) if (p + c.direction * c.brain.speed) == pos =>
          c.walk((c.direction * c.brain.speed).length, walkEffort)
          c
      }

      val thingInPosition: Option[Thing] = world.gen(pos).flatMap {
        case creature: Creature => staying(world, creature, pos)
        case other              => Some(other)
      }

      val creaturesGoingHere = if (thingInPosition.exists(_.isInstanceOf[Creature])) creaturesWanttoGoHere.filter(_.brain.agression > 0) else creaturesWanttoGoHere
      val collidingThings = creaturesGoingHere ++ thingInPosition
      val food = thingInPosition.collect{ case f: Food => f }
      val collidingCreatures = collidingThings.collect{ case c: Creature => c }

      // if (creaturesGoingHere.nonEmpty || thingInPosition.exists { _.isInstanceOf[Creature] }) {
      //   println(s"$pos: gh: $creaturesGoingHere, thip: $thingInPosition, coll: $collidingThings")
      // }

      // world.nextGen(pos) = world.gen(pos)
      if (collidingCreatures.nonEmpty) {
        val ca = collidingCreatures.filter(_.isAlive)
        if (ca.isEmpty) world.nextGen(pos) = None
        else {
          val winner = fight(collidingCreatures)
          // if (collidingCreatures.size >= 2) println("  fight!")
          food.map{ winner.eat }
          world.nextGen(pos) = winner
        }
      }
      else {
        assert(!thingInPosition.exists{ _.isInstanceOf[Creature] })
        world.nextGen(pos) = thingInPosition
      }
    }

    world.swapBuffers()
  }

  def fight(creatures: Set[Creature]): Creature = {
    val winner = creatures.maxBy(c => c.energy * c.brain.agression)
    val killed = creatures - winner
    winner.energy += killed.map(c => if (c.brain.agression > 0) -c.energy else c.energy).sum
    winner
  }

  def staying(world: World, creature: Creature, pos: Vec2, checked: Set[Creature] = Set.empty): Option[Creature] = {
    if (checked contains creature) return None
    val wantedPos = world.gen.clamp(pos + creature.direction * creature.brain.speed)
    if (wantedPos == pos) Some(creature)
    else {
      world.gen(wantedPos).flatMap {
        case other: Creature if creature.brain.agression <= 0 => staying(world, other, wantedPos, checked + creature).map(_ => creature)
        case other: Creature if creature.brain.agression > 0 => None
        // case other: Creature => None // no blocking, always fight
        case food: Food => None // creature will move to the food
        case _ => Some(creature) // Something is blocking so creature cannot move away
      }
    }
  }

  var nextDraw = Duration.Zero.fromNow

  def writeDOTToFile(world: World) {
    if (world.gen.creatures.isEmpty) return

    if (nextDraw.timeLeft <= Duration.Zero) {
      nextDraw = 5 seconds fromNow
      Future {
        val oldest = world.gen.creatures.values.maxBy(_.age)
        File.write("/tmp/oldest.dot", DOTExport.toDOT(oldest.genotype.expand, Brain.inputs, Brain.outputs, Brain.labelNames))
        File.write("/tmp/oldestg.dot", DOTExport.toDOT(oldest.genotype))
      }
    }
  }

  def mutate(genotype: Grammar[Double, Double], mutationCount: Int): Grammar[Double, Double] = {
    var curr = genotype
    for (_ <- 0 to mutationCount) {
      val mut = random.select(mutationOperators)
      curr = mut(curr).getOrElse(curr)
    }

    curr
  }
}
