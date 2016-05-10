package aiolia

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Compiler {
  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import universe._
  def apply[T](tree: universe.Tree): T = {
    val toolbox = currentMirror.mkToolBox()
    val compiledCode = toolbox.compile(tree)
    val result = compiledCode()
    result.asInstanceOf[T]
  }
}
