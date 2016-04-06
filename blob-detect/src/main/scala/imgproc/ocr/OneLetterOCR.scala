package imgproc.ocr

import imgproc.{AnswerMatrixMeasures, ImageProcessing}
import imgproc.ImageProcessing._
import org.opencv.contrib.Contrib
import org.opencv.core._
import org.opencv.imgproc.Imgproc

import scala.collection.JavaConverters._

/**
 * Created by alvaro on 20/01/16.
 */
object OneLetterOCR {

  case class LetterProb(char: Char, probability: Double) {
    assert(probability >= 0 && probability <= 1)
  }
  
  type LetterResult = Seq[LetterProb]

  def average[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
    num.toDouble( ts.sum ) / ts.size
  }


  def thresholdLettersImage(m: Mat) = canny()(meanShift()(m))


  def mergeBoundingBoxes(points: Seq[MatOfPoint])(offset: Int = 2) = {
    import imgproc.Implicits._
    import scala.util.control.Breaks._

    val bBoxes = scala.collection.mutable.ArrayBuffer() ++ points.map( _.boundingBox )

    var finish = false
    while( !finish ){
      finish = true
      breakable {
        for (i <- 0 until bBoxes.size; j <- i + 1 until bBoxes.size) {
          if (bBoxes(i).grow(offset) overlaps bBoxes(j)) {
            bBoxes(i) = bBoxes(i) union bBoxes(j)
            bBoxes.remove(j)
            finish = false
            break
          }
        }
      }
    }

    bBoxes.toSeq
  }

  val letterFragmentToCellRatio = 500

  def findContoursOfLetterFragment( m: Mat, minAreaForLetterFragment: Double = AnswerMatrixMeasures.cellArea/letterFragmentToCellRatio ) = {
    import imgproc.Implicits._
    findContours(m).filter( _.boundingBox.area > minAreaForLetterFragment )
  }

  def extractPossibleLettersBBox( m: Mat ) = {
    val thresholded = thresholdLettersImage(m)
    val contours = findContoursOfLetterFragment(thresholded)
    mergeBoundingBoxes(contours)()
  }

  def extractPossibleLettersImage( m: Mat ) = {
    import imgproc.Implicits._
    val contours = extractPossibleLettersBBox(m)
    contours.map( c => submatrix(m, (c.grow(2) intersection m.rect).get ))
  }


  def normalizeLetter(mat: Mat) : Mat = {
    val grayscale = toGrayscaleImage(mat)
    def gray = {

      val t = Core.mean(grayscale).`val`(0) - 1
      Imgproc.threshold(grayscale, grayscale, t, 255, Imgproc.THRESH_BINARY_INV)
      grayscale
    }

    def equalize = {
      Imgproc.equalizeHist(grayscale,grayscale)
      //Core.LUT(grayscale,lut,grayscale)
      //Contrib.applyColorMap(grayscale,grayscale,Contrib.COLORMAP_PINK)
      val white = new Mat(grayscale.rows(),grayscale.cols(),grayscale.`type`(),new Scalar(255))
      Core.subtract(white,grayscale,grayscale)
      grayscale
    }

    equalize
  }


  def scan( m: Mat ) : LetterResult = {

    val normalized = normalizeLetter(m)

    def howMuchDifferent(m: Mat): Double = {
      ???
    }

    for (p <- Pattern.patterns) yield {
      val differences = p.mats.map(howMuchDifferent)
      val minDifference = differences.max
      val avgDifference = average(differences)
      val probability = (1.0 / minDifference) max 1
      new LetterProb(p.letter, probability)
    }

  }

  def apply( m: Mat ) = scan(m)

}
