package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.{PrintStream, File}

import imgproc.steps.ProcessingStep
import org.junit.runner.RunWith
import org.opencv.highgui.Highgui
import imgproc.ImageProcessing._

import org.opencv.core.Mat
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import scala.util.Try


@RunWith(classOf[JUnitRunner])
class ProcessingStepTest extends FlatSpec {

  nu.pattern.OpenCV.loadLibrary()

  import imgproc.Implicits._


  private val testImgPath = {

    def remove(f: File): Unit = if (f.isFile) {
      f.delete()

    }
    else {
      val files = f.listFiles
      if (files != null) {
        files.foreach(remove)
      }
    }

    val p = new File("./build/test-img")
    remove(p)
    p.mkdir()
    p
  }

  private def testImgPath(file: String): File = new File(testImgPath, file)

  private val positiveMatchImages = Seq(
    "2016-01-26-101322.jpg",
    "2016-01-26-101343.jpg",
    "2016-01-26-101403.jpg",
    "2016-01-26-101423.jpg",
    "2016-01-26-101448.jpg",
    "2016-01-26-101502.jpg",
    "2016-01-26-101516.jpg"
  )


  import ProcessingStep._
  import ProcessingStep.Implicits._

  private def processMat(step: ProcessingStep, m: Mat) = step.process(m).mat.get

  private def saveTestImage(name: String, m: Mat) = Highgui.imwrite(testImgPath(name).toString, m)


  private def runSomeTestAndFailIfSoMuchFailures[T](files: Seq[String], allowedFailureRatio: Double = 0.2)(test: String => T): Unit = {

    def runSomeTestAndCollectFailures[T](files: Seq[String])(test: String => T) = {
      val results = for (f <- files) yield (f, Try(test(f)))
      results.filter(_._2.isFailure)
    }

    def reportFailures[T](failures: Seq[(String, Try[T])], out: PrintStream = System.out) {
      for ((file, failure) <- failures) {
        out.println(file)
        failure.failed.get.printStackTrace(out)
      }
    }

    val failures = runSomeTestAndCollectFailures(files)(test)
    reportFailures(failures)
    if (failures.size > allowedFailureRatio * files.size) {
      fail(s"To much failures: ratio:$allowedFailureRatio failures:${failures.size} files:${files.size}")
    }
  }


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
      val m2 = processMat(contourStep.withDrawContours(i => Some(i.contours)), m)
      saveTestImage("04-contours-" + imageLocation, m2)
    }
  }

  "Quadrilateral filter step" should "extract quadrilaterals" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(quadrilateralStep.withDrawContours(i => Some(i.quadrilaterals)), m)
      saveTestImage("05-quads-" + imageLocation, m2)
    }
  }

  "Biggest quadrilaterals step" should "find quadrilaterals" in {
    for (imageLocation <- positiveMatchImages) {
      val m = readImageFromResources(imageLocation)
      val m2 = processMat(biggestQuadrilateralsStep.withDrawContours(i => Some(i.biggestQuadrilaterals)), m)
      saveTestImage("06-bigquads-" + imageLocation, m2)
    }
  }

  {
    behavior of "Answer location step"

    it should "find a location" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val location = answerMatrixLocationStep.process(m).location
        assert(location.isDefined, imageLocation)
      }
    }

    it should "find a location and save image" in {
      runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
        val m = readImageFromResources(imageLocation)
        val m2 = processMat(answerMatrixLocationStep.withDrawContours(i => i.location.map(r => Seq(r))), m)
        saveTestImage("07-answerlocation-" + imageLocation, m2)
      }
    }
  }

  "Answer matrix extraction step" should "extract matrix" in {
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val extracted = processMat(answerMatrixStep, m)
      saveTestImage("08-extracted-" + imageLocation, extracted)
    }
  }

  "Cells extraction step" should "extract cells" in {
    runSomeTestAndFailIfSoMuchFailures(positiveMatchImages) { imageLocation =>
      val m = readImageFromResources(imageLocation)
      val extracted = processMat(cellsOfAnswerMatrix.withDrawContours(_.cells.map(s => s.map(rectToMatOfPoint))), m)
      saveTestImage("09-cells-" + imageLocation, extracted)
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
        val extracted2 = processMat(studentInfoStep, extracted)
        println(imageLocation)
        saveTestImage("11-studentinfoagain-" + imageLocation, extracted2)
      }

    }
  }


  class MoreThanOneException(throwables: Seq[Throwable]) extends RuntimeException("More than one exception:" + throwables.mkString("\n")) {
  }

}

