package imgproc.ocr

import imgproc.ImageProcessing
import imgproc.ImageProcessing._
import org.opencv.core.{MatOfPoint, Core, Mat}
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


  def thresholdLettersImage(m: Mat) = canny()(meanShift(m))


  def mergeBoundingBoxes(points: Seq[MatOfPoint])(offset: Int = 3) = {
    import imgproc.Implicits._
    import scala.util.control.Breaks._

    val bboxes = scala.collection.mutable.ArrayBuffer() ++ points.map( _.boundingBox )

    var finish = false
    while( !finish ){
      finish = true
      breakable {
        for (i <- 0 until bboxes.size; j <- i + 1 until bboxes.size) {
          if (bboxes(i).grow(offset) overlaps bboxes(j)) {
            bboxes(i) = bboxes(i) add bboxes(j)
            bboxes.remove(j)
            finish = false
            break
          }
        }
      }
    }

    bboxes.toSeq
  }

  def extractPossibleLettersBBox( m: Mat ) = {
    val thresholded = thresholdLettersImage(m)
    val contours = findContours(thresholded)
    mergeBoundingBoxes(contours)()
  }

  def normalizeLetter(mat: Mat) : Mat = ???

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
