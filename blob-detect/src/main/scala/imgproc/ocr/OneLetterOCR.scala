package imgproc.ocr

import javax.imageio.ImageIO

import com.typesafe.scalalogging.slf4j.LazyLogging
import imgproc.ocr.Pattern.TrainingPatterns
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

    val minProbability = 0.35
    val minGap = 0.15

    val bestProbability = results.maxBy( _.probability )
    val significative = bestProbability.probability >= minProbability && results.forall{
      case LetterProb(c,p) => c == bestProbability.char || p + minGap < bestProbability.probability
    }
    val prediction : Option[Char] = if(significative) Some(bestProbability.char) else None
    def description = results.map( lp => f"${lp.char}${lp.probability}%2.5f" ).mkString("-")
    override def toString = "prediction:" + prediction + " -- " + results.toString
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

  def findContoursOfLetterFragment( m: Mat, minAreaForLetterFragment: Double = AnswerMatrixMeasures(None,1).cellArea/letterFragmentToCellRatio ) = {
    import imgproc.Implicits._

    findContours(m).filter( _.boundingBox.area > minAreaForLetterFragment )
  }

  def extractPossibleLettersBBox( m: Mat ) = {
    val thresholded = thresholdLettersImage(m)
    val contours = findContoursOfLetterFragment(thresholded)
    val bboxes = mergeBoundingBoxes(contours)()


    val amm = AnswerMatrixMeasures(None,1)
    import amm.Params._

    val filters : Seq[ Rect => Boolean ] = Seq(
      _.width > cellSize.w.w/15,
      _.width < cellSize.w.w/2,
      _.height > cellSize.h.h/3
    )

    filters.foldLeft(bboxes)( (b,f) => b.filter(f) )
  }

  def extractPossibleLettersImage( m: Mat, bboxGrow: Int = 3 ) = {
    import imgproc.Implicits._
    val contours = extractPossibleLettersBBox(m)
    contours.map( c => submatrix(m, (c.grow(bboxGrow) intersection m.rect).get ))
  }


  def normalizeLetter(mat: Mat,offset: Int = 10) : Mat = {

    def gray = {
      val grayscale = toGrayscaleImage(mat)
      val t = Core.mean(grayscale).`val`(0) - 1
      Imgproc.threshold(grayscale, grayscale, t, 255, Imgproc.THRESH_BINARY_INV)
      grayscale
    }

    def invertGrayscale( m: Mat ) = {
      val white = new Mat(m.rows(),m.cols(),m.`type`(),new Scalar(255))
      Core.subtract(white,m,m)
      m
    }

    val equalize = {
      Imgproc.equalizeHist(gray,gray)
      //Core.LUT(grayscale,lut,grayscale)
      //Contrib.applyColorMap(grayscale,grayscale,Contrib.COLORMAP_PINK)
      //invertGrayscale(grayscale)
      val t = Core.mean(gray).`val`(0)
      Imgproc.threshold(gray,gray,t+offset,255,Imgproc.THRESH_TRUNC)
      invertGrayscale(gray)
      Imgproc.threshold(gray,gray,t+offset,-1,Imgproc.THRESH_TOZERO)
      gray
    }

    Pattern.resizeToPatterSize(equalize)
  }

}



abstract class OneLetterOCR  extends LazyLogging{
  import OneLetterOCR._
  import imgproc.Implicits._

  protected val perceptron : Perceptron

  def predict( pattern: Mat ) : LetterResult = {
    val e = extractPossibleLettersImage(pattern)
    if( e.size == 0 ){
      perceptron.predict(normalizeLetter(pattern))
    }
    else {
      val mat = e.maxBy(_.area)
      perceptron.predict(normalizeLetter(mat))
    }
  }
}

class TrainedOneLetterOCR(trainingPatterns: TrainingPatterns = Pattern.letterTrainingPatterns ) extends OneLetterOCR{
  import OneLetterOCR._

  logger.error( s"trainingPatterns: ${trainingPatterns}")

  val perceptron = new Perceptron()

  val normalizedTrainingPatterns = trainingPatterns.map{ case(c,mats) =>
    c -> mats.map(normalizeLetter(_))
  }

  logger.error( s"normalizedTrainingPatterns: ${normalizedTrainingPatterns}")


  perceptron.train(normalizedTrainingPatterns)
}

class CrossRecognizer(trainingPatterns: TrainingPatterns = Pattern.crossTrainingPatterns) extends OneLetterOCR{
  import OneLetterOCR._

  val perceptron = new Perceptron()

  val normalizedTrainingPatterns = trainingPatterns.map{ case(c,mats) =>
    c -> mats.map(normalizeLetter(_))
  }

  logger.error( s"normalizedTrainingPatterns: ${normalizedTrainingPatterns}")

  perceptron.train(normalizedTrainingPatterns)
}
