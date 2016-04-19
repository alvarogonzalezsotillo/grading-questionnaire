package imgproc.ocr

import imgproc.ocr.perceptron.Perceptron
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
  
  implicit class LetterResult( results: Seq[LetterProb] ){

    val best = results.maxBy( _.probability ).char
    val significative = {
      true
    }
    val prediction : Option[Char] = if(significative) Some(best) else None
    def description = results.map( lp => String.format("%s%2.2f", lp.char, lp.probability) ).mkString("-")
    override def toString = "best:" + best + " -- " + results.toString
  }


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

  val letterFragmentToCellRatio = 400

  def findContoursOfLetterFragment( m: Mat, minAreaForLetterFragment: Double = AnswerMatrixMeasures.cellArea/letterFragmentToCellRatio ) = {
    import imgproc.Implicits._

    findContours(m).filter( _.boundingBox.area > minAreaForLetterFragment )
  }

  def extractPossibleLettersBBox( m: Mat ) = {
    val thresholded = thresholdLettersImage(m)
    val contours = findContoursOfLetterFragment(thresholded)
    val bboxes = mergeBoundingBoxes(contours)()


    import AnswerMatrixMeasures._

    val filters : Seq[ Rect => Boolean ] = Seq(
      _.width > (cellWidth - cellHeaderWidth)/10,
      _.width < (cellWidth - cellHeaderWidth)/3,
      _.height > cellHeight/2
    )

    filters.foldLeft(bboxes)( (b,f) => b.filter(f) )
  }

  def extractPossibleLettersImage( m: Mat ) = {
    import imgproc.Implicits._
    val contours = extractPossibleLettersBBox(m)
    contours.map( c => submatrix(m, (c.grow(3) intersection m.rect).get ))
  }


  def normalizeLetter(mat: Mat) : Mat = {
    val grayscale = toGrayscaleImage(mat)

    def gray = {
      val t = Core.mean(grayscale).`val`(0) - 1
      Imgproc.threshold(grayscale, grayscale, t, 255, Imgproc.THRESH_BINARY_INV)
      grayscale
    }

    def invertGrayscale( m: Mat ) = {
      val white = new Mat(grayscale.rows(),grayscale.cols(),grayscale.`type`(),new Scalar(255))
      Core.subtract(white,m,m)
      m
    }

    def equalize = {
      Imgproc.equalizeHist(grayscale,grayscale)
      //Core.LUT(grayscale,lut,grayscale)
      //Contrib.applyColorMap(grayscale,grayscale,Contrib.COLORMAP_PINK)
      //invertGrayscale(grayscale)
      val t = Core.mean(grayscale).`val`(0)
      Imgproc.threshold(grayscale,grayscale,t,255,Imgproc.THRESH_TRUNC)
      invertGrayscale(grayscale)
      Imgproc.threshold(grayscale,grayscale,t,-1,Imgproc.THRESH_TOZERO)
      grayscale
    }

    equalize
  }

}



abstract class OneLetterOCR{
  import OneLetterOCR._

  protected val perceptron : Perceptron

  def predict( pattern: Mat ) : LetterResult = perceptron.predict( normalizeLetter(pattern) )
}

class TrainedOneLetterOCR extends OneLetterOCR{
  import OneLetterOCR._

  val perceptron = new Perceptron()

  val normalizedTrainingPatterns = Pattern.trainingPatterns.map{ case(c,mats) =>
    c -> mats.map(normalizeLetter)
  }

  perceptron.train(normalizedTrainingPatterns)
}
