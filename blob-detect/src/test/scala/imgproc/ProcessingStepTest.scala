package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.File

import imgproc.ProcessingStep.ProcessingStepInfo
import org.junit.runner.RunWith
import org.opencv.highgui.Highgui
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ProcessingStepTest extends FlatSpec {

  nu.pattern.OpenCV.loadLibrary()

  def readImageFromResources( f: String ) = {
    val url = getClass().
      getResource(f).
      getPath
    Highgui.imread(url)
  }

  val testImgPath = {
    val p = new File("./test-img")
    p.mkdir()
    p
  }

  def testImgPath( file: String ) : File = new File( testImgPath, file )

  val imageLocation = "2015-10-09-093120.jpg" // "2015-10-09-093035.jpg"

  "Initial step " should "do nothing" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.initialStep.processMat(m).mat

    assert( m eq m2)
  }

  "threshold step" should "convert to grayscale" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.thresholdStep.processMat(m).mat
    Highgui.imwrite(testImgPath("threshold-" + imageLocation).toString, m2)
  }


  "noise reduction step" should "reduce noise" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.noiseReductionStep.processMat(m).mat
    Highgui.imwrite(testImgPath("noisereduction-"+imageLocation).toString, m2)
  }

  "biggest quadrilaterals step" should "find quadrilaterals" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.biggestQuadrilateralsStep.withDrawContours.processMat(m).mat
    Highgui.imwrite(testImgPath("bigquad-"+imageLocation).toString, m2)
  }


  "Answer location step" should "find a location" in {
    val m = readImageFromResources(imageLocation)
    val location = ProcessingStep.answerMatrixLocationStep.processMat(m).info
    assert( location.isDefined )
  }

  "Answer location step" should "find a location and save image" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.answerMatrixLocationStep.withDrawContour.processMat(m).mat
    Highgui.imwrite(testImgPath("answerlocation-"+imageLocation).toString, m2)
  }

  "Answer matrix extraction step" should "extract matrix" in {
    val m = readImageFromResources(imageLocation)
    val location = ProcessingStep.answerMatrixLocationStep.processMat(m).info
    val extracted = ProcessingStep.answerMatrixStep.process( new ProcessingStepInfo(m,location) ).mat
    Highgui.imwrite(testImgPath("extracted-"+imageLocation).toString, extracted)
  }


}

