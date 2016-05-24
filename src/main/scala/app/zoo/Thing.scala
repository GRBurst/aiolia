package aiolia.app.zoo.world

import aiolia.grammar.Grammar
import aiolia.graph.DSL._
import aiolia.neuralNetwork.Recurrent
import aiolia.util.Vec2

sealed trait Thing {
  protected def initialPos: Vec2
  private var _pos: Vec2 = initialPos
  private[world] def pos_=(pos: Vec2) { _pos = pos }
  def pos = _pos
}

sealed trait Food extends Thing {
  val energy: Double
}

class Apple(protected val initialPos: Vec2) extends Food {
  val energy = 0.5
  override def toString = s"Apple($energy)"
}

object Creature {
  def apply(genotype: Grammar[Double, Double], initialEnergy: Double, pos: Vec2) = new Creature(genotype, initialEnergy, pos)
}

class Creature(val genotype: Grammar[Double, Double], initialEnergy: Double, protected val initialPos: Vec2) extends Thing {
  import Brain._

  val brain = new Brain(genotype)
  energy = initialEnergy

  private var _energy: Double = _
  def energy_=(newEnergy: Double) { _energy = 0.0 max newEnergy min 1.0 }
  def energy = _energy

  private var _direction: Vec2 = Vec2.up
  private def direction_=(newDirection: Vec2) { _direction = newDirection }
  def direction = _direction

  private var _age: Long = 0
  private def age_=(newAge: Long) { _age = newAge }
  def age = _age

  def isAlive = energy > 0
  def canReplicate = brain.horniness > 0 && energy > 0.5 && age > 1

  def think(sensors: Array[Double], effort: Double) {
    brain.feed(energy, sensors)
    brain.think()
    direction = direction.rotate(brain.rotation)
    age += 1
    energy -= Math.log(brain.size) * effort
  }

  def walk(distance: Double, effort: Double) {
    energy -= distance * effort
  }

  def eat(food: Food) {
    energy += food.energy
  }

  def replicate(): Double = {
    if (brain.horniness <= 0) return 0

    val passedOnEnergy = brain.horniness * energy
    energy -= passedOnEnergy
    passedOnEnergy
  }

  override def toString = s"Creature(energy = $energy, direction = $direction, replication = $replicate, pos = $pos)"
}

object Brain {
  val inputs = VL(0 to 8)
  val outputs = VL(9, 10, 11, 12)
}

class Brain(net: Recurrent) {
  def this(g: Grammar[Double, Double]) = this(Recurrent(Brain.inputs, Brain.outputs, g.expand))

  def think() { net.think() }
  def feed(energy: Double, sensors: Array[Double]) = net.setInputData(energy :: sensors.toList)
  def size = net.size

  private val out = new {
    def rotation = net.outputData(0)
    def speed = net.outputData(1)
    def horniness = net.outputData(2)
    def agression = net.outputData(3)
  }

  private def thirds(x: Double) = if (x < -0.3) -1 else if (x > 0.3) 1 else 0

  def rotation: Double = out.rotation * Math.PI
  def speed: Int = thirds(out.speed)
  def horniness: Double = out.horniness
  def agression: Double = thirds(out.agression)
}
