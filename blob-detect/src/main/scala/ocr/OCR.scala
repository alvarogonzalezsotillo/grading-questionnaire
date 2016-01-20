package ocr

import imgproc.ImageProcessing
import org.opencv.core.{Rect, Mat}

/**
 * Created by alvaro on 20/01/16.
 */
object OCR {

  type OCRResult = Seq[OneLetterOCR.LetterProb]

  def detectLetterShapes(mat: Mat) : Seq[Rect]= ???

  def scan(m: Mat) : OCRResult = {
    val rects = detectLetterShapes(m)
    rects.map(r => OneLetterOCR(ImageProcessing.submatrix(m, r)))
  }

  def apply(m:Mat) = scan(m)
}
