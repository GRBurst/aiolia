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

class Creature(val genotype: Grammar[Double, Double], initialEnergy: Double, protected val initialPos: Vec2) extends Thing {
  import Creature._

  val brain = Recurrent(inputs, outputs, genotype.expand)
  energy = initialEnergy

  private var _energy: Double = _
  private def energy_=(newEnergy: Double) { _energy = 0.0 max newEnergy min 1.0 }
  def energy = _energy

  private var _age: Long = 0
  private def age_=(newAge: Long) { _age = newAge }
  def age = _age

  def direction: Vec2 = getDirection(brain)
  def replicationStrength: Double = getReplicationStrength(brain)

  def isAlive = energy > 0
  def canReplicate = replicationStrength > 0 && energy > 0.5 && age > 1 //TODO: rename to wantsToReplicate?

  def think(sensors: Array[Double], effort: Double) {
    brain.setInputData(Creature.inputData(energy, sensors))
    brain.think()
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
    if (replicationStrength <= 0) return 0

    val passedOnEnergy = replicationStrength * energy
    energy -= passedOnEnergy
    passedOnEnergy
  }

  override def toString = s"Creature(energy = $energy, direction = $direction, replication = $replicationStrength, pos = $pos)"
}

object Creature {
  val inputs = VL(0 to 8)
  val outputs = VL(9 to 11)
  def inputData(energy: Double, sensors: Array[Double]) = energy :: sensors.toList
  def getDirection(net: Recurrent): Vec2 = Vec2(net.outputData(0).signum, net.outputData(1).signum)
  def getReplicationStrength(net: Recurrent): Double = -1.0 max net.outputData(2) min 1.0
  def apply(genotype: Grammar[Double, Double], initialEnergy: Double, pos: Vec2) = new Creature(genotype, initialEnergy, pos)
}
