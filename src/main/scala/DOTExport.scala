package aiolia.export

import aiolia.Grammar
import aiolia.graph._
import collection.mutable

object DOTExport {
  def toDOT[V,E](graph: Graph[V,E]) = { import graph._
    s"""
digraph G {
  ${edges.mkString("\n  ")}
  ${vertices.map(v => s"$v [shape = circle]").mkString("\n  ")}
}
"""
  }

  // render with
  // dot -Tsvg -Kfdp input.dot -o output.svg
  // (fdp layout engine has cluster support)
  def toDOT[V,E](grammar: Grammar[V,E]) = { import grammar._
    val g = uniqueVertices

    // define graph clusters for axiom and rhs of production rules
    // (contains clusters for nonterminals)
    def graph(graphId: String, graph: Graph[V, E]) = {
      s"""
  subgraph cluster$graphId {
    style = filled
    color = "${if (graphId == "Axiom") "#369AFF" else "#BBBBBB"}"
    fillcolor = "${if (graphId == "Axiom") "#C9E4FF" else "#EEEEEE"}"
    label = "$graphId"
    ${graph.connectors.map(c => s"${c.label} [style=dashed]").mkString("\n    ")}
    ${graph.edges.map(e => s"$e").mkString("\n    ")}
    ${graph.vertices.map(v => s"$v [shape = circle]").mkString("\n    ")}
    ${nts(graphId, graph.nonTerminals)}
  }"""
    }

    // define nonterminal clusters
    def nts(graphId: String, ns: List[NonTerminal]): String = {
      // GraphViz cannot handle overlapping clusters
      // So we draw the overlapping nonterminals as their string representation
      val used = mutable.HashSet.empty[Vertex]

      val fillColor = "#BCFFC4"
      val color = "#49EA5C"

      ns.zipWithIndex.map{
        case (nonTerminal @ NonTerminal(label, conn), i) =>
          if ((used intersect conn.toSet).nonEmpty)
            s""""$nonTerminal" [style = filled, color = "$color", fillcolor = "$fillColor", shape = rect]"""
          else {
            used ++= conn
            s"""
    subgraph cluster${graphId}_nt${label}_${i} {
      style = filled
      color = "$color"
      fillcolor = "$fillColor"
      label = "${label}"
      ${conn.mkString(", ")}
    }"""
          }
      }.mkString("\n\n    ")
    }

    val graphs = ("Axiom" -> g.axiom) :: g.productions.toList.sortBy(_._1).map{ case (label, graph) => (s"$label" -> graph) }
    s"""
digraph Grammar {
  ${graphs.map{ case (label, gr) => graph(label, gr) }.mkString("\n")}


  ${
      // interconnect nonterminal- with rule-connectors
      graphs.flatMap{
        case (label, graph) => graph.nonTerminals.zipWithIndex.map{
          case (nt, i) =>
            (if (nt.connectors.isEmpty)
              s"""cluster${label}_nt${nt.label}_$i -> cluster${nt.label} [style=dashed]\n"""
            else "") +
              (nt.connectors zip g.productions(nt.label).connectors).map{ case (c1, c2) => s"$c1 -> $c2 [style=dashed]" }.mkString("\n  ")
        }
      }.mkString("\n  ")
    }
}
  """
  }
}
