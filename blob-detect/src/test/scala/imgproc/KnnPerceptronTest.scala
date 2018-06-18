package imgproc

import java.awt.image.RenderedImage
import java.io.File
import javax.imageio.ImageIO

import common.TestUtil
import imgproc.ImageProcessing._
import imgproc.Implicits._
import TestUtil._
import imgproc.ocr.OneLetterOCR._
import imgproc.ocr.Pattern.TrainingPatterns
import imgproc.ocr._
import imgproc.ocr.perceptron.{PerceptronWithSquareInput, UntrainedKnnPerceptron}
import imgproc.steps.AnswersInfo.cells
import imgproc.steps.ProcessingStep
import imgproc.steps.ProcessingStep.Implicits._
import org.junit.runner.RunWith
import org.opencv.core._
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import scala.util.Random

@RunWith (classOf[JUnitRunner] )
class KnnPerceptronTest extends FlatSpec {


  nu.pattern.OpenCV.loadLibrary()


  behavior of "Knn perceptron"

  it should " work" in {
    val p = new UntrainedKnnPerceptron with PerceptronWithSquareInput {
      override def patternSize: Int = Pattern.patternSize
    }

    p.train( Pattern.letterTrainingPatterns )

    val letter = 'a'
    val pattern = readImageFromResources(s"to-recognize-$letter.png")
    val prediction = p.predict(pattern)
    println( prediction )
  }

}
