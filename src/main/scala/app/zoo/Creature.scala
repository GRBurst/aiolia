package aiolia.app.zoo.creature

import aiolia.graph._

sealed trait Node
case class Neuron(bias: Double) extends Node
case class DistanceSensor(range: Double) extends Node
case class FoodSensor(range: Double, energy: Double) extends Node
case class CreatureSensor(range: Double, energy: Double) extends Node
case class Joint(minAngle: Double, maxAngle: Double) extends Node

sealed trait Edge
case class Synapse(weight: Double) extends Edge
case class Bone(length: Double) extends Edge

class Creature(graph: Graph[Node, Edge]) {
  // def brain = inducedSubGraph(v => vertexData(v).isInstanceOf[Neuron]).filterEdges(e => edgeData(e).isInstanceOf[Synapse]).mapData(
  //   vd => vd.asInstanceOf[Neuron].bias,
  //   ed => ed.asInstanceOf[Synapse].weight
  // )

  def brain = graph.collectData ({
    case Neuron(bias) => bias
  }, {
    case Synapse(weight) => weight
  })
}
