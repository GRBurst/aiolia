package aiolia.util

trait DoubleBuffering[B] {
  def bufferInit: B
  private val buffers = IndexedSeq.fill(2)(bufferInit)
  private var currentBuffer = 0

  def buffer = buffers(1 - currentBuffer)
  def nextBuffer = buffers(currentBuffer)

  def swapBuffers() { currentBuffer = 1 - currentBuffer }
}
