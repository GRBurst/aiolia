package aiolia.graph

object DSL {
  //TODO: graph dsl: LinkedHashSet for predictable vertex/edge traversal in tests
  //TODO: graph dsl: G as alias for Graph
  //TODO: implicits for accessors: V,E,NT,C, ....

  // Graph construction
  def v(label: Label) = Vertex(label)
  def V(labels: Range) = labels.map(Vertex(_)).toSet
  def V(labels: Label*) = labels.map(Vertex(_)).toSet
  def VL(labels: Label*) = labels.map(Vertex(_)).toList
  def e(edge: (Label, Label)) = edge match { case (in, out) => Edge(Vertex(in), Vertex(out)) }
  def E(es: (Label, Label)*) = es.map{ case (in, out) => Edge(Vertex(in), Vertex(out)) }.toSet
  def C(labels: Label*) = labels.map(Vertex(_)).toList // connectors

  def vData[V](data: (Int, V)*) = data.map { case (label, datum) => Vertex(label) -> datum }.toMap
  def eData[E](data: ((Int, Int), E)*) = data.map { case ((a, b), datum) => e(a -> b) -> datum }.toMap

  // NonTerminal
  def nt(l: Label, c: Product = None) = {
    assert(!c.isInstanceOf[List[_]], s"don't put Lists in NT($l, $c). Use a Tuple instead: NT($l, ${c.asInstanceOf[List[_]].mkString("(", ",", ")")})")
    NonTerminal(l, c.productIterator.toList.asInstanceOf[List[Int]].map(Vertex(_)))
  }
}
