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
    productions.map{
      case (l, g) => s"  [$l:${g.connectors.mkString("-")}] -> G(V(${g.vertices}), E(${g.edges})" +
        (if (g.vertexData.nonEmpty) s", {${g.vertexData.toList.sortBy(_._1.label).map{ case (v, d) => s"$v: $d" }.mkString(", ")}}" else "") +
        (if (g.edgeData.nonEmpty) s", {${g.edgeData.toList.sortBy(_._1.out.label).sortBy(_._1.in.label).map{ case (Edge(in, out), d) => s"$in->$out: $d" }.mkString(", ")}}" else "") +
        (if (g.nonTerminals.nonEmpty) s", NTS(${g.nonTerminals.mkString(", ")})" else "") +
        ")"
    }.mkString("\n")
  }\n)"

  // render with
  // dot -Tsvg -Kfdp input.dot -o output.svg
  def toDOT = {
    val g = this.uniqueVertices
    // val dep = g.dependencyGraph
    s"""
digraph Grammar {
  subgraph clusterAxiom {
    style = "filled"
    color=blue;
    fillcolor=lightgrey;
    label = "Axiom";
    ${g.axiom.edges.mkString("\n    ")}
    ${
      g.axiom.nonTerminals.zipWithIndex.map{
        case (nt, i) =>

          s"""subgraph clusterAxiom_nt${nt.label}_$i {
    style=filled;
    color=black;
      fillcolor=red;
      label = "nt ${nt.label}";
      ${nt.connectors.mkString(", ")}
    }"""
      }.mkString("\n\n      ")
    }
  }

  ${
      g.productions.map{
        case (label, graph) =>
          s"""subgraph cluster$label {
    style=filled;
    fillcolor=lightgrey;
    label = "rule $label";
    ${graph.edges.mkString("\n    ")}
    ${graph.connectors.map(c => s"${c.label} [style=dashed]").mkString("\n    ")}
    ${
            graph.nonTerminals.zipWithIndex.map{
              case (nt, i) =>

                s"""subgraph cluster${label}_nt${nt.label}_$i {
    style=filled;
      fillcolor=red;
      label = "nt ${nt.label}";
      ${nt.connectors.mkString(", ")}
    }"""
            }.mkString("\n\n      ")
          }
  }"""
      }.mkString("\n\n  ")
    }

    ${
      g.axiom.nonTerminals.zipWithIndex.map{
        case (nt, i) => s"""clusterAxiom_nt${nt.label}_$i -> cluster${nt.label}"""
      }.mkString("\n  ")
    }
    ${
      productions.flatMap{
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
