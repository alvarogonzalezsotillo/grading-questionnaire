package ocr

import imgproc.ImageProcessing._
import org.opencv.core.{Rect, Mat}

/**
 * Created by alvaro on 20/01/16.
 */
object OCR {

  type OCRResult = Seq[OneLetterOCR.LetterResult]

  def detectLetterShapes(mat: Mat): Seq[Rect] = {
    import imgproc.Implicits._
    val thresholded = threshold()(mat)
    val contours = findContours(thresholded).map(Shape)
    val minArea = mat.area / 4
    val candidates = contours.filter(_.boundingBox.area() > minArea)
    candidates.map(_.boundingBox)
  }

  def scan(m: Mat): OCRResult = {
    val rects = detectLetterShapes(m)
    rects.map(r => OneLetterOCR(submatrix(m, r)))
  }

  def apply(m: Mat) = scan(m)
}
