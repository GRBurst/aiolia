package aiolia.app.zoo

import aiolia.grammar._
import aiolia.neuralNetwork._
import aiolia.util._
import aiolia.graph.DSL._

import collection.mutable

case class Vec2(x: Int, y: Int) {
  def +(other: Vec2) = Vec2(x + other.x, y + other.y)
}

trait Food {
  val energy: Double
}

object Apple extends Food {
  val energy = 0.5
}

class Creature(val genotype: Grammar[Double, Double], initialEnergy: Double, var position: Vec2) {
  import Creature._

  val brain = Recurrent(inputs, outputs, genotype.expand)
  energy = initialEnergy

  private var _energy: Double = _
  def energy_=(newEnergy: Double) {
    _energy = 0.0 max newEnergy min 1.0
    Creature.setEnergy(brain, energy)
  }
  def energy = _energy

  var _age: Int = 0
  def age = _age
  private def age_=(newAge: Int) { _age = newAge }

  def replicationStrength = getReplicationStrength(brain)

  def act(sensors: Array[Double]): Vec2 = {
    Creature.setSensors(brain, sensors)
    brain.think()
    age += 1
    getDirection(brain)
    val direction = getDirection(brain)
    energy -= (direction.x.abs + direction.y.abs) * 0.001
    direction
  }

  def eat(food: Food) {
    energy += food.energy
  }

  def symbol: String = {
    val i = (energy * 10).toInt
    if (i == 10) "C" else i.toString
  }
}

object Creature {
  val inputs = VL((0 to 8).toList :_*)
  val outputs = VL(9, 10, 11)
  def setEnergy(net: Recurrent, energy: Double) = net.setInputData(0, energy * 2 - 1)
  def setSensors(net: Recurrent, sensors: Array[Double]) = sensors.zipWithIndex.foreach { case (sensor, i) =>
    net.setInputData(i + 1, sensor)
  }
  def inputList(energy: Double, sensors: Array[Double]) = energy :: sensors.toList
  def getDirection(net: Recurrent): Vec2 = Vec2(net.outputData(0).signum, net.outputData(1).signum)
  def getReplicationStrength(net: Recurrent): Double = net.outputData(2)
}

class Place(val position: Vec2) {
  var creature: Option[Creature] = None
  val foods: mutable.Set[Food] = mutable.HashSet.empty

  def isEmpty = creature.isEmpty && foods.isEmpty
  override def toString = {
    val desc = creature.map(c => c.symbol) orElse foods.headOption.map(_ => "F")
    desc.getOrElse(" ")
  }
}

class World(val dimensions: Vec2) extends Iterable[Place] {
  val field: Array[Array[Place]] = Array.tabulate(dimensions.x, dimensions.y)((x,y) => new Place(Vec2(x,y)))
  val creatures: mutable.Set[Creature] = mutable.HashSet.empty

  def apply(position: Vec2): Place = field(position.x)(position.y)

  def clamp(position: Vec2): Vec2 = Vec2(0 max position.x min (dimensions.x - 1), 0 max position.y min (dimensions.y - 1))
  def isInside(position: Vec2): Boolean = clamp(position) == position //TODO: optimize

  override def toString = field.map(_.mkString).mkString("\n")

  override def iterator = field.flatten.iterator
}

object Zoo extends App {
  val initialBrain = FeedForward
  val zoo = new LivingZoo(new ZooConfig)
  zoo.live()
}

class LivingZoo(config: ZooConfig) {
  import config._

  def baseGenotype: Grammar[Double,Double] = Grammar.inOutWithData(Creature.inputs, Creature.outputs, initVertexData)
  def initialEnergy: Double = 0.8
  def newCreature(position: Vec2) = new Creature(mutate(baseGenotype), initialEnergy, position)
  def randomPosition(world: World): Vec2 = Vec2(random.r.nextInt(world.dimensions.x), random.r.nextInt(world.dimensions.y))

  def assureThings(world: World) {
    if (world.creatures.size < 10) {
      val position = randomPosition(world)
      val place = world(position)
      if (place.creature.isEmpty) {
        val creature = newCreature(position)
        world.creatures += creature
        place.creature = Some(creature)
      }
    }

    val rand = random.r.nextDouble
    if (rand > 0.7) {
      val position = randomPosition(world)
      world(position).foods += Apple
    }
  }

  val directions = Array(Vec2(0,1), Vec2(1,0), Vec2(1,1), Vec2(0,-1), Vec2(-1,0), Vec2(1,-1), Vec2(-1,1), Vec2(-1,-1))

  def live() {
    val world = new World(worldDimensions)
    println("Initial world:")
    println(world)
    while (true) {
      assureThings(world)

      val newCreatures = mutable.HashSet.empty[Creature]
      world.creatures.foreach { creature =>
        val currentPlace = world(creature.position)

        currentPlace.foods.headOption.foreach { food =>
          creature.eat(food)
          currentPlace.foods -= food
        }

        val replicationStrength = creature.replicationStrength
        if (replicationStrength > 0) {
          // println("REPLICATION " + replicationStrength)
          val childPosition = directions.map(creature.position + _).filter(world.isInside).find(pos => world(pos).creature.isEmpty)
          childPosition.foreach { pos =>
            val passedOnEnergy = replicationStrength * creature.energy
            creature.energy -= passedOnEnergy
            val childCreature = new Creature(mutate(creature.genotype), passedOnEnergy, pos)
            newCreatures += childCreature
            world(pos).creature = Some(childCreature)
          }
        }

        val sensors = directions.map(creature.position + _).map { pos =>
          if (world.isInside(pos)) {
            val place = world(pos)
            val creatureScore = place.creature.map(_ => 0.5).getOrElse(0.0)
            val foodScore = place.foods.headOption.map(_ => 0.5).getOrElse(0.0)
            creatureScore + foodScore
          } else {
            -1
          }
        }
        val direction = creature.act(sensors)
        if (creature.energy > 0) {
          val newPosition = world.clamp(creature.position + direction)
          val newPlace = world(newPosition)
          if (newPlace.creature.isEmpty) {
            currentPlace.creature = None
            creature.position = newPlace.position
            newPlace.creature = Some(creature)
          }
        } else {
          currentPlace.creature = None
          world.creatures -= creature
        }
      }

      world.creatures ++= newCreatures

      if (world.creatures.nonEmpty) {
        val oldest = world.creatures.maxBy(_.age)
        File.write("oldest.dot", DOTExport.toDOT(oldest.genotype.expand, Creature.inputs, Creature.outputs))
      }

      println(world)
      println("#" * 10)
      Thread.sleep(200)
    }
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

class ZooConfig(
  val mutationCount: Int = 20,
  val worldDimensions: Vec2 = Vec2(25,25)
) extends DataGraphGrammarOpConfig[Double, Double] { config =>

  val random = Random(0)

  val mutationOperators: List[(Grammar[Double,Double]) => Option[Grammar[Double,Double]]] = (
    2 -> AddConnectedVertex(config) ::
    2 -> AddEdge(config) ::
    1 -> MutateVertex(config) ::
    1 -> MutateEdge(config) ::
    // 1 -> ExtractNonTerminal(config) ::
    // 1 -> ReuseNonTerminalAcyclic(config) ::
    // 1 -> InlineNonTerminal(config) ::
    Nil
).flatMap{ case (n, op) => List.fill(n)(op) }

  override def initVertexData() = Some(random.r.nextDouble * 2 - 1)
  override def initEdgeData() = Some(random.r.nextDouble * 2 - 1)
  override def mutateVertexData(d: Double) = d + random.r.nextGaussian
  override def mutateEdgeData(d: Double) = d + random.r.nextGaussian
}
