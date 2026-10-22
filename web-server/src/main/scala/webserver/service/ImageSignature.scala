package webserver.service

import org.opencv.core.{Mat, Size}
import org.opencv.imgproc.Imgproc

/**
  * A very small "signature" of an image, used to tell whether two images show the same
  * questionnaire or not (the camera does not move, so the same questionnaire gives almost
  * the same signature).
  *
  * It is the mean grayscale value of a very downscaled version of the image.
  */
case class ImageSignature(values: Array[Double]) {

  /** The mean absolute difference with another signature (0 means identical) */
  def distanceTo(other: ImageSignature): Double = {
    require(values.length == other.values.length)
    var total = 0.0
    var i = 0
    while (i < values.length) {
      total += Math.abs(values(i) - other.values(i))
      i += 1
    }
    total / values.length
  }

}

object ImageSignature {

  private val Columns = 32
  private val Rows = 24

  def of(image: Mat): ImageSignature = {
    val converted = if (image.channels() == 1) None else Some(new Mat())
    val gray = converted.getOrElse(image)
    val small = new Mat()

    try {
      converted.foreach(c => Imgproc.cvtColor(image, c, Imgproc.COLOR_BGR2GRAY))
      Imgproc.resize(gray, small, new Size(Columns, Rows))

      val buffer = new Array[Byte](Columns * Rows)
      small.get(0, 0, buffer)

      new ImageSignature(buffer.map(b => (b & 0xFF).toDouble))
    } finally {
      small.release()
      converted.foreach(_.release())
    }
  }

}
