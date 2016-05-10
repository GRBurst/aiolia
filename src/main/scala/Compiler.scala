package aiolia

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

//TODO: provide compile options to mkToolBox
object Compiler {
  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import universe._
  val toolbox = currentMirror.mkToolBox()

  def apply[T](tree: universe.Tree): T = {
    toolbox.compile(tree)().asInstanceOf[T]
  }
}
