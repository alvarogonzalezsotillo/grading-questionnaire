package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.{File, PrintStream}

import imgproc.steps.{AnswersInfo, ProcessingStep}
import org.junit.runner.RunWith
import org.opencv.highgui.Highgui
import imgproc.ImageProcessing._
import org.opencv.core.Mat
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import imgproc.Implicits._
import TestUtil._
import ProcessingStep._
import ProcessingStep.Implicits._
import imgproc.steps.AnswersInfo.{answers, cells, cellsLocation}
import imgproc.steps.ContoursInfo._
import imgproc.steps.LocationInfo.location
import imgproc.steps.MainInfo.{mat, originalMat}
import imgproc.steps.QRInfo.{answerMatrixMeasures, qrLocation}



@RunWith(classOf[JUnitRunner])
class ProcessingStepTest extends FlatSpec {

  nu.pattern.OpenCV.loadLibrary()

  import imgproc.steps.ProcessingStep.Implicits._

  private def processMat(step: ProcessingStep, m: Mat) = step.process(m)(mat).get



  val positiveMatchImages = /* Seq(
    "2016-01-26-101322.jpg",
    "2016-01-26-101343.jpg",
    "2016-01-26-101403.jpg",
    "2016-01-26-101423.jpg",
    "2016-01-26-101502.jpg",
    "2016-01-26-101516.jpg", */ Seq(
    "horizontal-ticked.png"
  )


  "Initial step " should "do nothing" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(initialStep, m)
      saveTestImage("01-original-" + imageLocation, m2)
      assert(m eq m2)
    }
  }

  "threshold step" should "convert to grayscale" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(thresholdStep, m)
      saveTestImage("02-threshold-" + imageLocation, m2)
    }
  }


  "noise reduction step" should "reduce noise" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(noiseReductionStep, m)
      saveTestImage("03-noisereduction-" + imageLocation, m2)
    }
  }

  "Contour extraction step" should "extract contours" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(contourStep.withDrawContours(_(contours)), m)
      saveTestImage("04-contours-" + imageLocation, m2)
    }
  }

  "Quadrilateral filter step" should "extract quadrilaterals" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(quadrilateralStep.withDrawContours(_(quadrilaterals)), m)
      saveTestImage("05-quads-" + imageLocation, m2)
    }
  }

  "Biggest quadrilaterals step" should "find quadrilaterals" in {
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(biggestQuadrilateralsStep.withDrawNumberedContours(_(biggestQuadrilaterals)), m)
      saveTestImage("06-bigquads-" + imageLocation, m2)
    }
  }

  {
    behavior of "Answer location step"

    it should "find a location" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val loc = answerMatrixLocationStep.process(m)(location)
        assert(loc.isDefined, s"loc.isDefined is false: $imageLocation")
      }
    }

    it should "find a location and save image" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val m2 = processMat(answerMatrixLocationStep.withDrawContours(_(location).map(r => Seq(r))), m)
        saveTestImage("07-answerlocation-" + imageLocation, m2)
      }
    }
  }

  {
    behavior of "QR step"

    it should "locate QR" in{
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val extracted = processMat(locateQRStep.withDrawContours( _(qrLocation).map( c => Seq(c) )), m)
        saveTestImage("08-qrlocation-" + imageLocation, extracted)
      }
    }

    it should "decode QR" in{
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        processMat(decodeQRStep, m)
      }
    }
  }

  "Answer columns step" should "find the columns of answers" in{
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(answerColumnsStep.withDrawNumberedContours(_(answerColumns)), m)
      saveTestImage("10-columns-" + imageLocation, m2)
    }
  }

  "Cells extraction (column based) step" should "find the cells" in{
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(cellsLocationStep.withDrawNumberedContours(_(cellsLocation)), m)
      saveTestImage("11-cells-" + imageLocation, m2)
    }
  }

  "Cells extraction (column based) step" should "save all the individual cells" in{
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val info = cellsLocationStep.process(m)
      for( measures <- info(answerMatrixMeasures) ;
           cells <- info(cellsLocation) ;
           answers <- info(answers);
           mat <- info(originalMat);
           _ = saveTestImage("12-kk.png",mat);
           questions = answers.size ;
           ((cellInMatrix,cell),index) <- cells.zip(measures.answerCells(questions)).zipWithIndex ) {
        val src = cellInMatrix
        val dst = measures.Params.cellSize.toRect.toOpenCV // cell.toOpenCV
        val h = ImageProcessing.findHomography(src,dst)
        println( s"$imageLocation: $index src:${src.points.mkString(",")} dst:${dst.points.mkString(",")} " )
        val cellM = ImageProcessing.warpImage()(mat,h,measures.Params.cellSize.toOpenCV)
        saveTestImage("12-cell-" + imageLocation + "-" + index + ".png", cellM )
      }
    }
  }

}

