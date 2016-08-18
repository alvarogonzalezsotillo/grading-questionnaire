package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.{File, PrintStream}

import imgproc.steps.{AnswersInfo, ProcessingStep}
import org.junit.runner.RunWith
import org.opencv.highgui.Highgui
import imgproc.ImageProcessing._
import org.opencv.core.{Mat, Point}
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
import imgproc.steps.QRInfo.{answerMatrixMeasures, qrLocation, qrText, qrVersion}



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
    "2016-01-26-101516.jpg", // Seq(
    "horizontal-letter.png",
    "vertical-letter.png",
    "horizontal-ticked.png",
    "vertical-ticked.png"
  )


  def removeFileExtension(s:String) = s.reverse.dropWhile(_!='.').tail.reverse

  def saveDerivedTestImage( imageLocation: String, stepName: String, m: Mat ) = {
    saveTestImage("processing-step/" + removeFileExtension(imageLocation) + "/" + stepName + ".png", m)
  }
  
  
  "Initial step " should "do nothing" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(initialStep, m)
      saveDerivedTestImage(imageLocation, "01-original", m2)
      assert(m eq m2)
    }
  }

  "threshold step" should "convert to grayscale" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(thresholdStep, m)
      saveDerivedTestImage(imageLocation, "02-threshold", m2)
    }
  }


  "noise reduction step" should "reduce noise" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(noiseReductionStep, m)
      saveDerivedTestImage(imageLocation, "03-noisereduction", m2)
    }
  }

  "Contour extraction step" should "extract contours" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(contourStep.withDrawContours(_(contours)), m)
      saveDerivedTestImage(imageLocation, "04-contours", m2)
    }
  }

  "Quadrilateral filter step" should "extract quadrilaterals" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(quadrilateralStep.withDrawContours(_(quadrilaterals)), m)
      saveDerivedTestImage(imageLocation, "05-quads", m2)
    }
  }

  "Biggest quadrilaterals step" should "find quadrilaterals" in {
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(biggestQuadrilateralsStep.withDrawNumberedContours(_(biggestQuadrilaterals)), m)
      saveDerivedTestImage(imageLocation, "06-bigquads", m2)

    }
  }

  {
    behavior of "QR step"

    it should "locate QR" in{
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val extracted = processMat(locateQRStep.withDrawContours( _(qrLocation).map( c => Seq(c) )), m)
        saveDerivedTestImage(imageLocation, "08-qrlocation", extracted)
      }
    }

    it should "decode QR" in{
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val info = decodeQRStep.process(m)
        assert( info(qrText).isDefined )
        println( info(qrText) )
      }
    }
  }

  "Answer columns step" should "find the columns of answers" in{
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages,true) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(answerColumnsStep.withDrawNumberedContours(_(answerColumns)), m)
      saveDerivedTestImage(imageLocation, "10-colums", m2)
    }
  }

  "Cells extraction (column based) step" should "find the cells" in{
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      import ImageProcessing.drawString

      val m = readImageFromResources(imageLocation)
      val info = cellsLocationStep.withDrawNumberedContours(_(cellsLocation)).process(m)
      val m2 = mat(info)
      val version = qrVersion(info)
      val ans = answers(info)
      val mMeasures = answerMatrixMeasures(info)

      drawString(m2,s"version:$version answers:$ans", new Point(20,20) )
      drawString(m2,s"measures:$mMeasures", new Point(20,60) )


      saveDerivedTestImage(imageLocation, "11-cells", m2)

    }
  }

  "Cells extraction (column based) step" should "save all the individual cells" in{
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val info = cellsStep.process(m)
      for( cellsMat <- info(cells) ; (mat,index) <- cellsMat.zipWithIndex ){
        saveDerivedTestImage(imageLocation, s"12-cell-$index", mat)

      }
    }
  }

}

