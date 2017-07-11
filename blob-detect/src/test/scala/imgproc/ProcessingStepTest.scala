package imgproc

/**
  * Created by alvaro on 8/07/15.
  */

import javax.imageio.ImageIO

import common.TestUtil
import imgproc.ImageProcessing._
import TestUtil._
import imgproc.steps.AnswersInfo.{answers, cells, cellsLocation, studentAnswers}
import imgproc.steps.ContoursInfo._
import imgproc.steps.MainInfo.{fileName, mat}
import imgproc.steps.{MainInfo, ProcessingStep}
import imgproc.steps.ProcessingStep._
import imgproc.steps.QRInfo.{answerMatrixMeasures, qrLocation, qrText, qrVersion}
import org.junit.runner.RunWith
import org.opencv.core.{Mat, Point}
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.words.ResultOfStringPassedToVerb


object ProcessingStepTest{
  val fromWebCam = Seq(

    "2017-03-03-122810.jpg",
    "2017-03-03-122820.jpg",
    "2017-03-03-122850.jpg",
    "2017-03-03-122856.jpg",
    "2017-03-03-122902.jpg",
    "2017-03-03-122909.jpg",
    "2017-03-03-122916.jpg",
    "2017-03-03-122923.jpg",
    "2017-03-03-122931.jpg",
    "2017-03-03-122935.jpg" )

  val  fromPDF = Seq(
    "horizontal-letter.png",
    "vertical-letter.png",
    "horizontal-ticked.png",
    "vertical-ticked.png"
  )

  val fromPDFFilled = Seq(
    "vertical-ticked-filled.png"
  )


}

@RunWith(classOf[JUnitRunner])
class ProcessingStepTest extends FlatSpec {

  import ProcessingStepTest._
  import imgproc.Implicits._

  nu.pattern.OpenCV.loadLibrary()

  import imgproc.steps.ProcessingStep.Implicits._

  private def processMat(step: ProcessingStep, m: Mat) = step.process(m)(mat).get



  private val positiveMatchImages = {
    import ProcessingStepTest._
      //fromPDF ++ fromWebCam ++
      // fromPDFFilled
      // fromWebCam

    Seq("2017-03-03-122810.jpg")
  }

  {


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
        val m2 = processMat(contourStep.withDrawContours(_ (contours)), m)
        saveDerivedTestImage(imageLocation, "04-contours", m2)
      }
    }

    "Quadrilateral filter step" should "extract quadrilaterals" in {
      for (imageLocation <- positiveMatchImages) {
        val m = readImageFromResources(imageLocation)
        val m2 = processMat(quadrilateralStep.withDrawContours(_ (quadrilaterals)), m)
        saveDerivedTestImage(imageLocation, "05-quads", m2)
      }
    }
  }


  "Biggest quadrilaterals step" should "find quadrilaterals" in {
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages,showFailures = true) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val initialInfo = infoFromMat(m)(fileName, imageLocation)
      val info = biggestQuadrilateralsStep.withDrawNumberedContours(_ (biggestQuadrilaterals)).process(initialInfo)
      val m2 = info(MainInfo.mat)
      m2.foreach(saveDerivedTestImage(imageLocation, "06-bigquads", _))

      val newM = m.clone
      for (pair <- info(allBiggestQuadrilaterals).map(_.zipWithIndex); (q, index) <- pair) {
        ImageProcessing.drawContours(newM, q)
        ImageProcessing.drawVertices(newM, q, index.toString)
      }
      saveDerivedTestImage(imageLocation, "06-allbigquads", newM)

      assert(info(allBiggestQuadrilaterals).isDefined)
      assert(info(biggestQuadrilaterals).isDefined)
    }
  }

  {
    behavior of "QR step"

    it should "locate QR" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val extracted = processMat(locateQRStep.withDrawContours(_ (qrLocation).map(c => Seq(c))), m)
        saveDerivedTestImage(imageLocation, "08-qrlocation", extracted)
      }
    }

    it should "decode QR" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages,true) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val info = decodeQRStep.process(m)
        assert(info(qrText).isDefined)
        println(info(qrText))
      }
    }
  }

  {
    "Answer columns step" should "find the columns of answers" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages, true) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val m2 = processMat(answerColumnsStep.withDrawNumberedContours(_ (answerColumns)), m)
        saveDerivedTestImage(imageLocation, "10-colums", m2)
      }
    }

    "Cells extraction (column based) step" should "find the cells" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>

        val m = readImageFromResources(imageLocation)
        val info = cellsLocationStep.withDrawNumberedContours(_ (cellsLocation)).process(m)
        val m2 = mat(info)

        if (true) {
          import ImageProcessing.drawString

          val version = qrVersion(info)
          val ans = answers(info)
          val mMeasures = answerMatrixMeasures(info)

          drawString(m2, s"version:$version answers:$ans", new Point(20, 20))
          drawString(m2, s"measures:$mMeasures", new Point(20, 60))
        }


        saveDerivedTestImage(imageLocation, "11-cells", m2)

      }
    }

    "Cells extraction (column based) step" should "save all the individual cells" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val info = cellsStep.process(m)
        for (cellsMat <- info(cells); (mat, index) <- cellsMat.zipWithIndex) {
          saveDerivedTestImage(imageLocation, s"12-cell-$index", mat)

        }
      }
    }


    "Student Answers Step" should "read answers" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages, true) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val info = studentAnswersStep.process(m)
        println(s"$imageLocation -> ${info(studentAnswers)}")
      }
    }
  }
}

