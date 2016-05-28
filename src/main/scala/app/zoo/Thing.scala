package aiolia.app.zoo.world

import aiolia.grammar.Grammar
import aiolia.graph.DSL._
import aiolia.graph._
import aiolia.neuralNetwork.Recurrent
import aiolia.util.{Vec2, AutoId}

sealed trait Thing {
  def appearance: Double
}

sealed trait Food extends Thing {
  val energy: Double
  val symbol: String
  def appearance = energy
}

object Apple extends Food {
  val energy = 0.5
  val symbol = "A"
  override def toString = s"Apple($energy)"
}

class Corpse(val creature: Creature) extends Food {
  val energy = creature.energy
  assert(energy > 0.0, energy)
  val symbol = "✝"
  override def toString = s"Corpse($creature)"
}

object Creature {
  def apply(genotype: Grammar[Double, Double], initialEnergy: Double) = new Creature(genotype, initialEnergy)
}

class Creature(val genotype: Grammar[Double, Double], initialEnergy: Double) extends Thing {
  import Brain._

  val brain = new Brain(genotype)
  assert(initialEnergy > 0.0)
  energy = initialEnergy
  def appearance = if (brain.agression > 0) -brain.agression else 0.2

  private var _energy: Double = _
  def energy_=(newEnergy: Double) { _energy = 0.0 max newEnergy min 1.0 }
  def energy = _energy

  private var _direction: Vec2 = Vec2.up
  private def direction_=(newDirection: Vec2) { _direction = newDirection }
  def direction = _direction

  private var _age: Long = 0
  private def age_=(newAge: Long) { _age = newAge }
  def age = _age
  var walkedDistance: Double = 0

  // def isAlive = energy > 0 && (age - walkedDistance < 15)
  def isAlive = energy > 0
  def wantsToReplicate = brain.horniness > 0 && energy > 0.8 && age > 5

  def think(sensors: Array[Double], effort: Double) {
    brain.feed(energy, sensors)
    brain.think()
    direction = direction.rotate(brain.rotation)
    age += 1
    energy -= Math.log(brain.size) * effort
  }

  def walk(distance: Double, effort: Double) {
    energy -= distance * effort
    walkedDistance += distance
  }

  def eat(food: Food) {
    energy += food.energy
  }

  def replicate(): Double = {
    val passedOnEnergy = brain.horniness * energy
    // val passedOnEnergy = 0.3 * energy
    energy -= passedOnEnergy
    passedOnEnergy
  }

  override def toString = s"C(${hashCode.toString.take(4)},d:${direction * brain.speed},e:${("%4.2f" format energy).drop(2)},a:${brain.agression})"
}

object Brain {
  val visionSensors = 4
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
