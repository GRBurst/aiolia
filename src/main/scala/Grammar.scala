package aiolia

import aiolia.helpers._
import aiolia.graph._
import aiolia.graph.types._

object Grammar {
  def minimal = Grammar(Graph(nonTerminals = List(NonTerminal(1))), Map(1 -> Graph()))
}

case class Grammar[+V, +E](axiom: Graph[V, E], productions: Map[Label, Graph[V, E]] = Map.empty[Label, Graph[V, E]]) {
  assert((0 until axiom.vertices.size).forall(axiom.vertices contains Vertex(_)), s"vertices need to have labels 0..|vertices|\n${axiom.vertices}") // needed for autoId in expand
  assert((productions.values.flatMap(_.nonTerminals) ++ axiom.nonTerminals).forall { nonTerminal =>
    val rhs = productions.get(nonTerminal.label)
    rhs.isDefined && (nonTerminal.connectors.size == rhs.get.connectors.size)
  }, "All existing nonterminals need to have an equivalent on the lhs")
  assert(!dependencyGraph.hasCycle, "this grammer contains cycles, which it shouldn't, so shit see this instead.")
  assert(axiom.connectors.isEmpty, "Axiom must not have connectors")
  assert(axiom.nonTerminals.nonEmpty, "Axiom must have at least one non-terminal")
  //TODO: either axiom has vertices or nonterminals

  //TODO: keine leeren hyperkanten auf der rhs in produktionen (nur in axiom erlaubt)

  def addProduction[V1 >: V, E1 >: E](production: (Label, Graph[V1, E1])): Grammar[V1, E1] = {
    assert(!(productions.keys.toSet contains production._1), s"productions already contain a rule with label ${production._1}\nproductions:\n${productions.mkString("\n")}")

    copy(productions = productions + production)
  }

  def removeProduction(nonTerminal: Label) = {
    // Recursive productions are not allowed, so check for all productions.
    assert(!productions.values.exists(_.nonTerminals.map(_.label) contains nonTerminal), "to-be-removed production is used in another production")
    assert(!(axiom.nonTerminals.map(_.label) contains nonTerminal), "to-be-removed production is used in axiom")
    copy(productions = productions - nonTerminal)
  }

  def updateProduction[V1 >: V, E1 >: E](production: (Label, Graph[V1, E1])): Grammar[V1, E1] = {
    val (label, graph) = production
    assert(productions contains label)
    copy(productions = productions.updated(label, graph))
  }

  def dependencyGraph = {
    val vertices = productions.keys.map(Vertex(_)).toSet
    val edges = productions.flatMap {
      case (lhs, rhs) =>
        rhs.nonTerminals.map(h => Edge(Vertex(lhs), Vertex(h.label)))
    }.toSet

    Graph(vertices, edges)
  }

  def cleanup: Grammar[V, E] = {
    val starts: List[Label] = axiom.nonTerminals.map(_.label).distinct
    val keep: Set[Vertex] = starts.flatMap(start => dependencyGraph.depthFirstSearch(Vertex(start))).toSet
    copy(productions = productions.filterKeys(keep contains Vertex(_)))
  }

  def uniqueVertices = {
    val autoId = AutoId(axiom.vertices.size)
    Grammar(axiom, productions.map{
      case (label, graph) =>
        val mapping = (graph.vertices.map(_.label) zip autoId).toMap
        label -> (graph map mapping)
    })
  }

  def expand = {
    var current = axiom
    val autoId = AutoId(axiom.vertices.size) // assuming that vertices have labels 0..n

    while (current.nonTerminals.nonEmpty) {
      // TODO: endless loop?
      // Mutation.mutate(Grammar.minimal, Random(6), 500)
      // println(current.vertices.size)
      val nonTerminal = current.nonTerminals.head
      val replacement = productions(nonTerminal.label)
      current = current.replaceOne(nonTerminal, replacement, autoId)
    }

    current
  }

  override def toString = s"Grammar(\n  Axiom: $axiom\n${
    productions.toList.sortBy(_._1).map{
      case (l, g) => s"  ${NonTerminal(l, g.connectors)} -> ${g.copy(connectors = Nil)}"
    }.mkString("\n")
  }\n)"

  // render with
  // dot -Tsvg -Kfdp input.dot -o output.svg
  def toDOT = {
    val g = this.uniqueVertices
    def graph(graphId: String, graph: Graph[V, E]) = {
      s"""
  subgraph cluster$graphId {
    style = "filled"
    color=${if (graphId == "Axiom") "blue" else "black"};
    fillcolor=lightgrey;
    label = "$graphId";
    ${graph.connectors.map(c => s"${c.label} [style=dashed]").mkString("\n    ")}
    ${graph.edges.mkString("\n    ")}
    ${nts(graphId, graph.nonTerminals)}
  }"""
    }

    def nts(graphId: String, ns: List[NonTerminal]) = {
      ns.zipWithIndex.map{
        case (NonTerminal(label, conn), i) =>
          s"""
    subgraph cluster${graphId}_nt${label}_${i} {
      style=filled;
      color=black;
      fillcolor=red;
      label = "nt ${label}";
      ${conn.mkString(", ")}
    }"""
      }.mkString("\n\n      ")
    }

    val graphs = ("Axiom" -> g.axiom) :: g.productions.map{ case (label, graph) => (s"$label" -> graph) }.toList
    s"""
digraph Grammar {
  ${graphs.map{ case (label, gr) => graph(label, gr) }.mkString("\n")}

  ${
      graphs.flatMap{
        case (label, graph) => graph.nonTerminals.zipWithIndex.map{
          case (nt, i) =>
            s"""cluster${label}_nt${nt.label}_$i -> cluster${nt.label}"""
        }
      }.mkString("\n  ")
    }
}
  """
  }
}
