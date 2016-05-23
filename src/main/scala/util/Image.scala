package aiolia.util

import java.awt.Dimension
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import net.coobird.thumbnailator.makers.FixedSizeThumbnailMaker
import net.coobird.thumbnailator.resizers.DefaultResizerFactory

object Constants {
  val sqrt3 = Math.sqrt(3.0)
  val sqrt3_255 = sqrt3 * 255.0
}

import Constants._

object RGB {
  def unapply(rgb: RGB24): Option[(Int, Int, Int)] = Some((rgb.r, rgb.g, rgb.b))
  def apply(rgb: Int) = RGB24(rgb)
  def apply(r: Int, g: Int, b: Int) = new RGB24(r, g, b)
}

final case class RGB24(rgb: Int) {
  def this(r: Int, g: Int, b: Int) = this((r << 16) | (g << 8) | b)
  def r = rgb >> 16
  def g = (rgb >> 8) & 0xFF
  def b = rgb & 0xFF

  def distanceSq(that: RGB24): Int = {
    val dr = this.r - that.r
    val dg = this.g - that.g
    val db = this.b - that.b
    dr * dr + dg * dg + db * db
  }

  def distance(that: RGB24): Int = Math.sqrt(distanceSq(that)).toInt //TODO: optimize with integer square root algorithm
  def distanceNormalized(that: RGB24): Double = distance(that).toDouble / sqrt3_255
}

object Image {
  def create(w: Int, h: Int) = new Image(new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB))
  def read(filename: String) = new Image(ImageIO.read(new java.io.File(filename)))
  def readResource(filename: String) = new Image(ImageIO.read(getClass.getResource(filename)))
}

class Image(val im: BufferedImage) {
  def w = im.getWidth
  def h = im.getHeight
  def pixels = w * h

  def getPixelRGB(x: Int, y: Int): RGB24 = {
    RGB24(im.getRGB(x, y))
  }
  def setPixelRGB(x: Int, y: Int, color: RGB24) = {
    im.setRGB(x, y, color.rgb)
  }

  def fill(f: (Double, Double) => Array[Double]) = {
    for (y <- 0 until h; x <- 0 until w) {
      val a = f(x.toDouble * 2 / w - 1, y.toDouble * 2 / h - 1) // 0..w/h => range -1..1
      val r = a(0)
      val g = a(1)
      val b = a(2)
      def t(c: Double) = ((c * 128).toInt + 128).max(0).min(255) // range -1..1 => 0..255
      setPixelRGB(x, y, RGB(t(r), t(g), t(b)))
    }
    this
  }

  def write(filename: String) = {
    ImageIO.write(im, "png", new java.io.File(filename))
    this
  }

  def pixelColorDistance(that: Image): Double = {
    assert(this.w == that.w && this.h == that.h)
    var error = 0.0
    for (y <- 0 until h; x <- 0 until w) {
      val colorDistance = this.getPixelRGB(x, y) distanceNormalized that.getPixelRGB(x, y)
      error += colorDistance
    }
    error / pixels
  }

  val radius: Int = w / 4
  val radiusSq = radius * radius
  def fuzzyDistance(that: Image): Double = {
    assert(this.w == that.w && this.h == that.h)
    var error = 0.0
    for (y <- 0 until h; x <- 0 until w) {
      val ref = that.getPixelRGB(x, y)
      val minDistance = (for ( //TODO: restrict to circle with disc rasterization algorithm
        y2 <- 0.max(y - radius).min(h - 1) to 0.max(y + radius).min(h - 1);
        x2 <- 0.max(x - radius).min(w - 1) to 0.max(x + radius).min(w - 1);
        if (x - x2) * (x - x2) + (y - y2) * (y - y2) < radiusSq
      ) yield {
        val colorDistance = ref distanceNormalized this.getPixelRGB(x2, y2)
        val spatialDistance = Math.sqrt((x - x2) * (x - x2) + (y - y2) * (y - y2)) / radius
        // println(s"col: $colorDistance, spat: $spatialDistance")
        //colorDistance * colorDistance + spatialDistance
        -((1.0 / (1.0 + spatialDistance)) * (1.0 / (1.0 + colorDistance*colorDistance)))
        //Math.exp(colorDistance + spatialDistance)
      }).min
      error += minDistance
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

  def insert(other: Image, x: Int, y: Int) = {
    val g2d = im.createGraphics()
    g2d.setComposite(java.awt.AlphaComposite.getInstance(java.awt.AlphaComposite.SRC_OVER, 1));
    g2d.drawImage(other.im, x, y, null);
    g2d.dispose();
  }
}
