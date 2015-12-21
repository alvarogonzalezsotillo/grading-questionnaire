package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.io.File

import imgproc.ProcessingStep.ProcessingInfo
import org.junit.runner.RunWith
import org.opencv.highgui.Highgui

import org.opencv.core.Mat
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ProcessingStepTest extends FlatSpec {

  nu.pattern.OpenCV.loadLibrary()

  import imgproc.Implicits._

  def readImageFromResources(f: String) : Mat = {
    def mat() = {
      val url = getClass().
    	getResource(f).
	getPath
      println( "readImageFromResources:" + f + " --> " + url )
      Highgui.imread(url)
    }

    def imageio() = {
      javax.imageio.ImageIO.read( getClass().getResource(f) )
    }
    
    imageio
  }

  val testImgPath = {

    def remove( f: File ) : Unit = if( f.isFile ){
      f.delete()

    }
    else{
      val files = f.listFiles
      if( files != null ){
        files.foreach( remove )
      }
    }

    val p = new File("./test-img")
    remove(p)
    p.mkdir()
    p
  }

  def testImgPath(file: String): File = new File(testImgPath, file)

  val positiveMatchImages = Seq(
    "2015-10-09-093035.jpg",
    "2015-10-09-093047.jpg",
    "2015-10-09-093053.jpg",
    //"2015-10-09-093107.jpg",
    "2015-10-09-093120.jpg",
    "2015-10-09-093125.jpg",
    "2015-10-09-093133.jpg"
  )

  val imageLocation = "2015-10-09-093035.jpg" //"2015-10-09-093120.jpg"

  "Initial step " should "do nothing" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.initialStep.processMat(m).mat
    Highgui.imwrite(testImgPath("1-original-" + imageLocation).toString, m2)
    assert(m eq m2)
  }

  "threshold step" should "convert to grayscale" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.thresholdStep.processMat(m).mat
    Highgui.imwrite(testImgPath("2-threshold-" + imageLocation).toString, m2)
  }


  "noise reduction step" should "reduce noise" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.noiseReductionStep.processMat(m).mat
    Highgui.imwrite(testImgPath("3-noisereduction-" + imageLocation).toString, m2)
  }

  "Contour extraction step" should "extract contours" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.contourStep.withDrawContours.processMat(m).mat
    Highgui.imwrite(testImgPath("4-contours-" + imageLocation).toString, m2)
  }

  "Quadrilateral filter step" should "extract quadrilaterals" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.quadrilateralStep.withDrawContours.processMat(m).mat
    Highgui.imwrite(testImgPath("5-quads-" + imageLocation).toString, m2)
  }

  "Biggest quadrilaterals step" should "find quadrilaterals" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.biggestQuadrilateralsStep.withDrawContours.processMat(m).mat
    Highgui.imwrite(testImgPath("6-bigquads-" + imageLocation).toString, m2)
  }


  "Answer location step" should "find a location" in {
    val m = readImageFromResources(imageLocation)
    val location = ProcessingStep.answerMatrixLocationStep.processMat(m).info
    assert(location.isDefined)
  }

  "Answer location step" should "find a location and save image" in {
    val m = readImageFromResources(imageLocation)
    val m2 = ProcessingStep.answerMatrixLocationStep.withDrawContour.processMat(m).mat
    Highgui.imwrite(testImgPath("7-answerlocation-" + imageLocation).toString, m2)
  }

  "Answer matrix extraction step" should "extract matrix" in {
    for (imageLocation <- positiveMatchImages) {
      println( s"imageLocation:$imageLocation")
      val m = readImageFromResources(imageLocation)
      val extracted = ProcessingStep.answerMatrixStep().processMat(m).mat
      Highgui.imwrite(testImgPath("8-extracted-" + imageLocation).toString, extracted)
    }
  }

  "Cells extraction step" should "extract cells" in {
    for (imageLocation <- positiveMatchImages) {
      println( s"imageLocation:$imageLocation")
      val m = readImageFromResources(imageLocation)
      val extracted = ProcessingStep.cellsOfAnswerMatrix(40).withDrawContours.processMat(m).mat
      Highgui.imwrite(testImgPath("9-cells-" + imageLocation).toString, extracted)
    }
  }

}

