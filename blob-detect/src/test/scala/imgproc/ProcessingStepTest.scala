package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.{File, PrintStream}

import imgproc.steps.ProcessingStep
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
import imgproc.steps.AnswersInfo.{cells, cellsRect}
import imgproc.steps.ContoursInfo.{biggestQuadrilaterals, contours, quadrilaterals}
import imgproc.steps.LocationInfo.location
import imgproc.steps.MainInfo.mat
import imgproc.steps.QRInfo.qrLocation



@RunWith(classOf[JUnitRunner])
class ProcessingStepTest extends FlatSpec {

  nu.pattern.OpenCV.loadLibrary()

  import imgproc.steps.ProcessingStep.Implicits._

  private def processMat(step: ProcessingStep, m: Mat) = step.process(m)(mat).get



  val positiveMatchImages = Seq(
    "2016-01-26-101322.jpg",
    "2016-01-26-101343.jpg",
    "2016-01-26-101403.jpg",
    "2016-01-26-101423.jpg",
    "2016-01-26-101502.jpg",
    "2016-01-26-101516.jpg",
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
        assert(loc.isDefined, imageLocation)
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
    behavior of "Answer matrix extraction step"

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


    it should "extract matrix" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val extracted = processMat(answerMatrixStep, m)
        saveTestImage("08-extracted-" + imageLocation, extracted)
      }
    }
  }

  {
    behavior of "Cells extraction step"

    it should "extract cells" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val extracted = processMat(cellsOfAnswerMatrix.withDrawContours(_(cellsRect)), m)
        saveTestImage("09-cells-" + imageLocation, extracted)
      }
    }

    it should "save extracted individual cells" in{
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val info = cellsOfAnswerMatrix.process(m)
        val cellsOfInfo = info(cells).get.zipWithIndex
        for ((cell, index) <- cellsOfInfo) {
          saveTestImage(s"09-cell-${index+1}-" + imageLocation, cell)
        }
      }
    }

  }

  {
    behavior of "Student info step"

    it should "extract QR, student info and answer matrix" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val extracted = processMat(studentInfoStep, m)
        saveTestImage("10-studentinfo-" + imageLocation, extracted)
      }

    }

    it should "be enough to parse again" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages, 0.3) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val extracted = processMat(studentInfoStep, m)
        saveTestImage("11-studentinfoagain-qr-" + imageLocation, processMat(locateQRStep.withDrawContours( _(qrLocation).map( c => Seq(c) )),extracted) )
        val extracted2 = processMat(studentInfoStep, extracted)
        saveTestImage("11-studentinfoagain-" + imageLocation, extracted2)
      }

    }
  }



}

