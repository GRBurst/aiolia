package aiolia.graph

//TODO: make Vertex a value class? (problems with mockito)
// case class Vertex(label: Label) {
case class Vertex(label: Label) extends AnyVal {
  def map(vm: Label => Label) = Vertex(vm(label))

  override def toString = s"$label"
}

// case class Edge(in: Vertex, out: Vertex) {
object Edge {
  def apply(in: Vertex, out: Vertex): Edge = {
    assert(in != out, "Self loops are not allowed")
    Edge.fromLabels(in.label, out.label)
  }
  def fromLabels(inLabel: Label, outLabel: Label): Edge = {
    assert(inLabel != outLabel, "Self loops are not allowed")
    new Edge((inLabel.toLong << 32) | outLabel)
  }
  def unapply(edge: Edge): Option[(Vertex, Vertex)] = {
    Some((edge.in, edge.out))
  }
}

class Edge private (val inOut: Long) extends AnyVal {
  def inLabel: Label = (inOut >> 32).toInt
  def outLabel: Label = (inOut & 0xFFFFFFFF).toInt
  def in = Vertex(inLabel)
  def out = Vertex(outLabel)

  def contains(v: Vertex) = inLabel == v.label || outLabel == v.label
  def toSet = Set(in, out)
  def map(vm: Label => Label) = Edge.fromLabels(vm(inLabel), vm(outLabel))

  override def toString = s"${in.label} -> ${out.label}"

  // disabled for value class:
  // override def equals(other: Any) = other match {
  //   case that: Edge => this.inOut == that.inOut
  //   case _          => false
  // }
  // override def hashCode = inOut.hashCode
}

case class NonTerminal(label: Label, connectors: Array[Vertex] = Array.empty) {
  assert(connectors.sameElements(connectors.distinct), s"connectors in graph need to be distinct")

  def contains(v: Vertex) = connectors contains v
  def map(vm: Label => Label) = NonTerminal(label, connectors map (_ map vm))

  override def toString = s"[$label${if (connectors.nonEmpty) s":${connectors.mkString("-")}" else ""}]"
}
