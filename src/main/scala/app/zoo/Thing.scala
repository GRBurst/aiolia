package aiolia.app.zoo.world

import aiolia.grammar.Grammar
import aiolia.graph.DSL._
import aiolia.graph._
import aiolia.neuralNetwork.Recurrent
import aiolia.util.{Vec2, AutoId}

sealed abstract class Thing(initialPos: Vec2) {
  private var _pos: Vec2 = initialPos
  private[world] def pos_=(pos: Vec2) { _pos = pos }
  def pos = _pos
}

sealed abstract class Food(initialPos: Vec2) extends Thing(initialPos) {
  val energy: Double
  val symbol: String
}

class Apple(initialPos: Vec2) extends Food(initialPos) {
  val energy = 0.5
  val symbol = "A"
  override def toString = s"Apple($energy)"
}

class Corpse(val creature: Creature) extends Food(creature.pos) {
  protected val initialPos = creature.pos
  val energy = creature.energy
  val symbol = "✝"
  override def toString = s"Corpse($creature)"
}

object Creature {
  def apply(genotype: Grammar[Double, Double], initialEnergy: Double, pos: Vec2) = new Creature(genotype, initialEnergy, pos)
}

class Creature(val genotype: Grammar[Double, Double], initialEnergy: Double, initialPos: Vec2) extends Thing(initialPos) {
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
  var walkedDistance = 0

  def isAlive = energy > 0 && (age - walkedDistance < 15)
  def canReplicate = energy > 0.8 && age > 2

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
    // val passedOnEnergy = 0.3 * energy
    energy -= passedOnEnergy
    passedOnEnergy
  }

  override def toString = s"Creature(energy = $energy, direction = $direction, replication = ${brain.horniness}, pos = $pos)"
}

object Brain {
  val visionSensors = 3
  private val inAutoId = AutoId(0)
  private def inId = inAutoId.nextId
  private val _inMap: List[(Int, String)] = (
    (inId -> "enr") ::
    (0 until visionSensors).toList.flatMap(i => List(inId -> s"dst$i", inId -> s"ap$i")) :::
    Nil
  )
  private val inMap: Map[Label, String] = _inMap.toMap
  private val inLabels: List[Label] = _inMap.unzip._1
  println(inLabels)

  private val outAutoId = AutoId(inId)
  private def outId = outAutoId.nextId
  private val _outMap: List[(Int, String)] = (
    (outId -> "rot") ::
    (outId -> "spd") ::
    (outId -> "hrn") ::
    (outId -> "agr") ::
    Nil
  )
  private val outMap: Map[Label, String] = _outMap.toMap
  private val outLabels: List[Label] = _outMap.unzip._1

  val inputs = VL(inLabels: _*)
  val outputs = VL(outLabels: _*)
  val labelNames = inMap ++ outMap
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
