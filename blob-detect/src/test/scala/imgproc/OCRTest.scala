package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.File
import java.util

import imgproc.ocr.OneLetterOCR
import imgproc.steps.ProcessingStep
import imgproc.steps.ProcessingStep.Implicits._

import org.junit.runner.RunWith
import org.opencv.core.{Core, Mat, MatOfPoint, Point}
import org.opencv.utils.Converters
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import TestUtil._
import imgproc.Implicits._
import ImageProcessing._
import OneLetterOCR._


@RunWith(classOf[JUnitRunner])
class OCRTest extends FlatSpec {


  nu.pattern.OpenCV.loadLibrary()

  private def cellsOfTestImage( f: String ) = {
    val img = readImageFromResources( f )
    val info = ProcessingStep.cellsOfAnswerMatrix.process(img)
    info.cells.get
  }


  "Cells" should "be normalized" in {
    val fs = Seq("ocr-1.png","ocr-2.png")
    for( f <- fs) {
      val cells = cellsOfTestImage(f)
      for ((c, index) <- cells.zipWithIndex; (letter, lindex) <- extractPossibleLetters(c).zipWithIndex) {
        val n = normalizeLetter(c)
        val mean = Core.mean(c).`val`(0)

        saveTestImage(s"12-normalizedcell-${index + 1}-${lindex}-i-$mean-$f", c)
        saveTestImage(s"12-normalizedcell-${index + 1}-${lindex}-f-$f.jpg", n)
      }
    }
  }
}
