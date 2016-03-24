package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.File
import java.util

import imgproc.ocr.{Pattern, OneLetterOCR}
import imgproc.steps.ProcessingStep
import imgproc.steps.ProcessingStep.Implicits._

import org.junit.runner.RunWith
import org.opencv.core._
import org.opencv.imgproc.Imgproc
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

    def preciseThreshold(blockSize: Int = 3, C: Double = 0)(src: Mat): Mat = {
      val b = {
        val d = Pattern.patternSize/15
        if( d%2 == 1  ) d else d+1
      }
      clean(1,(b/2).toInt,(b/2).toInt)()(threshold(b,0)(src))
    }

    def preciseThreshold2(blockSize: Int = 3, C: Double = 0)(src: Mat): Mat = {
      val dst = new Mat(src.height(), src.width(), CvType.CV_8UC1)
      Imgproc.cvtColor(src, dst, Imgproc.COLOR_RGB2GRAY)


      //Imgproc.adaptiveThreshold(dst, dst, 255, Imgproc.ADAPTIVE_THRESH_MEAN_C, Imgproc.THRESH_BINARY_INV, blockSize, C)
      val mean = Core.mean(src).`val`(0)
      println( s"mean:$mean")
      val thresh = mean - 10 //235
      Imgproc.threshold(dst,dst,thresh,255,Imgproc.THRESH_BINARY)
      dst

    }

    def canny(t1: Int = 1, t2: Int = 20)( m: Mat ) = {
      val ret = new Mat
      Imgproc.Canny(m,ret,t1,t2)
      ret
    }

    def meanShift( m: Mat ) = {
      val ret = new Mat
      val spatialWindow = 100
      val colorWindow = 20

      Imgproc.pyrMeanShiftFiltering(m,ret,spatialWindow,colorWindow)
      ret
    }

    def normalizeLetter( m: Mat ) = {
      import ImageProcessing._
      val squared = stretchImage()(m,Pattern.patternSize,Pattern.patternSize)
      val thresholded = preciseThreshold()(squared)
      val cleaned = clean(3,3,3)()(thresholded)
      thresholded
    }



    val fs = Seq("ocr-1.png")
    for( f <- fs) {
      val cells = cellsOfTestImage(f)
      for ((c, index) <- cells.zipWithIndex; (letter, lindex) <- extractPossibleLetters(c).zipWithIndex) {

        if( index+1 == 33 || index+1 == 32 ) {
          saveTestImage(s"12-${index + 1}-$lindex-$f", c)
          saveTestImage(s"12-${index + 1}-$lindex-mean-$f", meanShift(c))
          saveTestImage(s"12-${index + 1}-$lindex-mean-canny-$f", canny()(meanShift(c)))
        }
      }
    }
  }
}
