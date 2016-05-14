package aiolia.util

import java.awt.Dimension
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import net.coobird.thumbnailator.makers.FixedSizeThumbnailMaker
import net.coobird.thumbnailator.resizers.DefaultResizerFactory

object Image {
  def create(w: Int, h: Int) = new Image(new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB))
  def read(filename: String) = new Image(ImageIO.read(new java.io.File(filename)))
}

class Image(im: BufferedImage) {
  def w = im.getWidth
  def h = im.getHeight
  def pixels = w * h

  def getPixel(x: Int, y: Int): Int = im.getRGB(x, y)
  def getPixelRGB(x: Int, y: Int): (Int, Int, Int) = {
    val c = getPixel(x, y)
    (c >> 16, (c >> 8) & 0xFF, c & 0xFF)
  }
  def setPixel(x: Int, y: Int, rgb: Int) = { im.setRGB(x, y, rgb); this }
  def setPixelRGB(x: Int, y: Int, r: Int, g: Int, b: Int) = {
    setPixel(x, y, (r << 16) | (g << 8) | b)
  }

  def fill(f: Array[Double] => Array[Double]) = {
    for (y <- 0 until h; x <- 0 until w) {
      val Array(r, g, b) = f(Array(x.toDouble * 2 / w - 1, y.toDouble * 2 / h - 1)) // 0..w/h => range -1..1
      def t(c: Double) = ((c * 128).toInt + 128).max(0).min(255) // range -1..1 => 0..255
      setPixelRGB(x, y, t(r), t(g), t(b))
    }
    this
  }

  def write(filename: String) = {
    ImageIO.write(im, "png", new java.io.File(filename))
    this
  }

  def distance(that: Image): Double = {
    assert(this.w == that.w && this.h == that.h)
    var error = 0.0
    for (y <- 0 until h; x <- 0 until w) {
      val (r1, g1, b1) = this.getPixelRGB(x, y)
      val (r2, g2, b2) = that.getPixelRGB(x, y)
      val colorDistance = Math.sqrt((r1 - r2) * (r1 - r2) +
        (g1 - g2) * (g1 - g2) +
        (b1 - b2) * (b1 - b2))
      val normalized = colorDistance / Math.sqrt(3) / 255
      error += normalized
    }
    error / pixels
  }

  def resized(_newW: Int, _newH: Int = -1): Image = {
    val newW = _newW.max(1)
    val newH = (if (_newH == -1) ((h.toDouble / w.toDouble) * newW).toInt else _newH).max(1)

    val resizer = DefaultResizerFactory.getInstance().getResizer(new Dimension(w, h), new Dimension(newW, newH))
    val scaledImage = new FixedSizeThumbnailMaker(newW, newH, false, true).resizer(resizer).make(im)

    new Image(scaledImage)
  }
}
