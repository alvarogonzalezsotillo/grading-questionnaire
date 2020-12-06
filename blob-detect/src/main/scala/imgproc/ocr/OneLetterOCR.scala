package imgproc.ocr

import java.io.File
import javax.imageio.ImageIO

import com.typesafe.scalalogging.LazyLogging
import imgproc.ocr.Pattern.TrainingPatterns
import imgproc.ocr.perceptron.{LetterPerceptron, Perceptron, PerceptronWithSquareInput, UntrainedAnnPerceptron}
import imgproc.{AnswerMatrixMeasures, ImageProcessing}
import imgproc.ImageProcessing._
import imgproc.ocr.OneLetterOCR.LetterResult
import imgproc.ocr.perceptron.LetterPerceptron.AnnPerceptronParams
import org.opencv.contrib.Contrib
import org.opencv.core.Core.MinMaxLocResult
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

  implicit class LetterResult(results: Seq[LetterProb]) {

    val minProbability = 0.35
    val minGap = 0.15

    val bestProbability = results.maxBy(_.probability)
    val significative = bestProbability.probability >= minProbability && results.forall {
      case LetterProb(c, p) => c == bestProbability.char || p + minGap < bestProbability.probability
    }
    val prediction: Option[Char] = if (significative) Some(bestProbability.char) else None

    def description = results.map(lp => f"${lp.char}${lp.probability}%2.5f").mkString("-")

    override def toString = "prediction:" + prediction + " -- " + results.toString
  }


  def average[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
    num.toDouble(ts.sum) / ts.size
  }


  def thresholdLettersImage(m: Mat) = canny()(meanShift()(m))


  def mergeBoundingBoxes(points: Seq[Rect])(offset: Int = 2) = {
    import imgproc.Implicits._
    import scala.util.control.Breaks._

    val bBoxes = scala.collection.mutable.ArrayBuffer() ++ points

    var finish = false
    while (!finish) {
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

  def findContoursOfLetterFragment(m: Mat, minAreaForLetterFragment: Double = AnswerMatrixMeasures(None, 1).cellArea / letterFragmentToCellRatio) = {
    val amm = AnswerMatrixMeasures(None, 1)
    import amm.params._
    import imgproc.Implicits._

    val contours = findContours(m).filter(_.boundingBox.area > minAreaForLetterFragment)

    val filtersForContours: Seq[MatOfPoint => Boolean] = Seq(
      _.width < cellSize.w.w / 4
    )

    filtersForContours.foldLeft(contours)((b, f) => b.filter(f))
  }

  def extractPossibleLettersBBox(m: Mat) = {
    val amm = AnswerMatrixMeasures(None, 1)
    import amm.params._
    import imgproc.Implicits._

    val thresholded = thresholdLettersImage(m)
    val contours = findContoursOfLetterFragment(thresholded).map(_.boundingBox)

    val bboxes = mergeBoundingBoxes(contours)()



    val filtersForBBoxes: Seq[Rect => Boolean] = Seq(
//      _.width > cellSize.w.w / 20,
//      _.width < cellSize.w.w / 5,
//      _.height > cellSize.h.h / 4
    )

    filtersForBBoxes.foldLeft(bboxes)((b, f) => b.filter(f))
  }

  def extractPossibleLettersImage(m: Mat, bboxGrow: Int = 3) = {
    import imgproc.Implicits._
    val contours = extractPossibleLettersBBox(m)
    contours.map(c => submatrix(m, (c.grow(bboxGrow) intersection m.rect).get))
  }


  def normalizeLetter(mat: Mat, offset: Int = 10): Mat = {

    val equalize = {

      val gray = {
        val grayscale = toGrayscaleImage(mat)
        val t = Core.mean(grayscale).`val`(0) - 1
        Imgproc.threshold(grayscale, grayscale, t, 255, Imgproc.THRESH_BINARY_INV)
        grayscale
      }

      def invertGrayscale(m: Mat) = {
        val white = new Mat(m.rows(), m.cols(), m.`type`(), new Scalar(255))
        Core.subtract(white, m, m)
        m
      }

      Imgproc.equalizeHist(gray, gray)
      //Core.LUT(grayscale,lut,grayscale)
      //Contrib.applyColorMap(grayscale,grayscale,Contrib.COLORMAP_PINK)
      //invertGrayscale(grayscale)
      val t = Core.mean(gray).`val`(0)
      Imgproc.threshold(gray, gray, t + offset, 255, Imgproc.THRESH_BINARY)
      invertGrayscale(gray)
      Imgproc.threshold(gray, gray, t + offset, -1, Imgproc.THRESH_TOZERO)
      gray
    }

    Pattern.resizeToPatterSize(equalize)
  }

  def normalizeTrainingPatterns(trainingPatterns: TrainingPatterns) = trainingPatterns.map { case (c, mats) =>
    c -> mats.map(normalizeLetter(_))
  }
}

abstract class OneLetterOCR extends LazyLogging {

  import OneLetterOCR._
  import imgproc.Implicits._

  protected val perceptron: UntrainedAnnPerceptron

  def predict(pattern: Mat): LetterResult = {
    if (DefaultEmptyRecognizer.isEmpty(pattern)) {
      Seq(LetterProb(' ', 1))
    }
    else {
      perceptron.predict(normalizeLetter(pattern))
    }
  }


}


class TrainedOneLetterOCR(trainingPatterns: TrainingPatterns, perceptronParams : AnnPerceptronParams = LetterPerceptron.defaultParams ) extends OneLetterOCR {

  import OneLetterOCR._

  logger.error(s"trainingPatterns: ${trainingPatterns}")

  protected val perceptron = LetterPerceptron(perceptronParams)

  val normalizedTrainingPatterns = normalizeTrainingPatterns(trainingPatterns)

  logger.error(s"normalizedTrainingPatterns: ${normalizedTrainingPatterns}")


  perceptron.train(normalizedTrainingPatterns)
}

object DefaultTrainedOneLetterOCR extends TrainedOneLetterOCR(Pattern.letterTrainingPatterns)

class CrossRecognizer(trainingPatterns: TrainingPatterns) extends OneLetterOCR {

  import OneLetterOCR._

  protected val perceptron = LetterPerceptron()

  val normalizedTrainingPatterns = normalizeTrainingPatterns(trainingPatterns)

  logger.error(s"normalizedTrainingPatterns: ${normalizedTrainingPatterns}")

  perceptron.train(normalizedTrainingPatterns)
}

object DefaultCrossRecognizer extends CrossRecognizer(Pattern.crossTrainingPatterns)

protected class EmptyRecognizer extends LazyLogging {

  import ImageProcessing._
  import OneLetterOCR._


  val patterns = {
    val allLetters = Pattern.letterTrainingPatterns.values.toSeq.flatten
    val allTicks = Pattern.crossTrainingPatterns.values.toSeq.flatten
    Map('A' -> (allLetters ++ allTicks)) ++ Pattern.emptyPatterns
  }

  val normalizedPatterns = normalizeTrainingPatterns(patterns)

  private val perceptron : Perceptron = {
    val p = LetterPerceptron()
    p.train(normalizedPatterns)
    p
  }

  def isEmpty(pattern: Mat): Boolean = {
    val prediction = perceptron.predict(normalizeLetter(pattern))
    logger.error( s"isEmpty:$prediction")
    prediction.prediction match {
      case Some('A') => false
      case Some(' ') => true
      case Some(_) => throw new IllegalStateException
      case None => true
    }

  }
}

object DefaultEmptyRecognizer extends EmptyRecognizer with App {
  nu.pattern.OpenCV.loadLibrary()
}