package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import imgproc.ImageProcessing._
import imgproc.TestUtil._
import imgproc.ocr.OneLetterOCR._
import imgproc.steps.ProcessingStep
import imgproc.steps.ProcessingStep.Implicits._
import org.junit.runner.RunWith
import org.opencv.core.{Rect, Scalar}
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import Implicits._


@RunWith(classOf[JUnitRunner])
class OCRTest extends FlatSpec {


  nu.pattern.OpenCV.loadLibrary()

  private def cellsOfTestImage(f: String) = {
    val img = readImageFromResources(f)
    val info = ProcessingStep.cellsOfAnswerMatrix.process(img)
    info.cells.get
  }


  {
    behavior of "bounding boxes"

    it should "find overlap if contained" in{
      val r1 = new Rect(0,0,10,10)
      val r2 = new Rect(1,1,2,2)
      assert( r1 overlaps r2)
      assert( r2 overlaps r1)
    }

    it should "find overlap if one corner inside" in{
      val r1 = new Rect(0,0,10,10)
      val r2 = new Rect(-1,-1,5,5)
      assert( r1 overlaps r2)
      assert( r2 overlaps r1)
    }

    it should "find overlap if two corners inside" in{
      val r1 = new Rect(0,0,10,10)
      val r2 = new Rect(-1,2,5,5)
      assert( r1 overlaps r2)
      assert( r2 overlaps r1)
    }

    it should "find overlap if no corners inside" in{
      val r1 = new Rect(0,0,10,10)
      val r2 = new Rect(-1,2,20,20)
      assert( r1 overlaps r2)
      assert( r2 overlaps r1)
    }

    it should "find no overlap if there is no overlap" in{
      val r1 = new Rect(0,0,10,10)
      val r2 = new Rect(-4,-4,2,2)
      assert( !(r1 overlaps r2) )
      assert( !(r2 overlaps r1) )
    }


  }

  {

    val fs = Seq("ocr-1.png", "ocr-2.png")

    behavior of "Cell"

    it should "be thresholded to find single letters" in {
      for (f <- fs; cells = cellsOfTestImage(f) ; (c, index) <- cells.zipWithIndex ){
          saveTestImage(s"12-${index + 1}-$f", c)
          saveTestImage(s"12-${index + 1}-lettersContours-$f", thresholdLettersImage(c))
      }
    }

    it should "find contours from thresholded image" in{
      for (f <- fs; cells = cellsOfTestImage(f) ; (c, index) <- cells.zipWithIndex ){
        val contours = findContours(thresholdLettersImage(c))
        drawContours(c, contours, new Scalar(255,0,255), 1 )
        saveTestImage(s"13-${index + 1}-$f", c)
      }
    }

    it should "find letter contours from thresholded image" in{
      for (f <- fs; cells = cellsOfTestImage(f) ; (c, index) <- cells.zipWithIndex ){
        val contours = extractPossibleLettersBBox(c)
        drawContours(c, contours.map( _.asShape ), new Scalar(255,0,255), 1 )
        saveTestImage(s"14-${index + 1}-$f", c)
      }
    }

  }
}
