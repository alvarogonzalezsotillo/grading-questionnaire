package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import imgproc.ImageProcessing.AnswerMatrixMeasures
import org.junit.runner.RunWith
import org.opencv.core.{Core, MatOfPoint, Point}
import org.opencv.imgproc.Imgproc
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ImageProcessingTest extends FlatSpec {

  import imgproc.Implicits._

  nu.pattern.OpenCV.loadLibrary()

  "The number of rows" should "be correct" in {
    assert(AnswerMatrixMeasures.rows(1) == 1)


    assert(AnswerMatrixMeasures.rows(5) == 1)
    assert(AnswerMatrixMeasures.rows(4) == 1)
    assert(AnswerMatrixMeasures.rows(6) == 2)


    assert(AnswerMatrixMeasures.rows(29) == 6)
    assert(AnswerMatrixMeasures.rows(30) == 6)
    assert(AnswerMatrixMeasures.rows(31) == 7)


  }



  "The homography found" should "yield the original points" in {

    val pointsInImage = new MatOfPoint((1.0, 1.0), (10.0, 1.0), (10.0, 10.0), (1.0, 10.0))

    val questions = 35
    val H = ImageProcessing.findHomography(questions)(pointsInImage)

    val dstPoints: MatOfPoint = new MatOfPoint( new Point, new Point, new Point, new Point)

    Core.perspectiveTransform(pointsInImage,dstPoints,H)


  }


}

