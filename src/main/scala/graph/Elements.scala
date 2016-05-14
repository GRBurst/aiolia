package aiolia.graph

//TODO: make Vertex a value class? (problems with mockito)
// case class Vertex(label: Label) extends AnyVal {
case class Vertex(label: Label) {
  def map(vm: Label => Label) = Vertex(vm(label))

  override def toString = s"$label"
}

case class Edge(in: Vertex, out: Vertex) {
  assert(in != out, "Self loops are not allowed")

  def contains(v: Vertex) = in == v || out == v
  def toSet = Set(in, out)
  def map(vm: Label => Label) = Edge(in map vm, out map vm)

  override def toString = s"${in.label} -> ${out.label}"
}

case class NonTerminal(label: Label, connectors: List[Vertex] = Nil) {
  assert(connectors == connectors.distinct, "connectors in graph need to be distinct")

  def contains(v: Vertex) = connectors contains v
  def map(vm: Label => Label) = NonTerminal(label, connectors map (_ map vm))

  override def toString = s"[$label${if (connectors.nonEmpty) s":${connectors.mkString("-")}" else ""}]"
}
