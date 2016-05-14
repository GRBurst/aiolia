package aiolia.util

object File {
  def write(filename: String, content: String) {
    Some(new java.io.PrintWriter(filename)).foreach{ p =>
      p.write(content)
      p.close()
    }
  }
}
