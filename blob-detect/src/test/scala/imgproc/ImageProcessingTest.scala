package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import java.util

import org.junit.runner.RunWith
import org.opencv.core.{Mat, Core, MatOfPoint, Point}
import org.opencv.imgproc.Imgproc
import org.opencv.utils.Converters
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ImageProcessingTest extends FlatSpec {

  import imgproc.Implicits._
  import scala.collection.JavaConversions._

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



  "The homography found" should "yield the original points (rectangle)" in {



    val pointsInImage = new MatOfPoint( (1.0, 1.0), (10.0, 1.0), (10.0, 10.0), (1.0, 10.0) )

    testHomography(pointsInImage)
  }


  "The homography found" should "yield the original points (romboid)" in {



    val pointsInImage = new MatOfPoint( (1.0, 1.0), (10.0, 10.10), (0.0, 20.0), (-10.0, 10.0) )

    testHomography(pointsInImage)
  }

  def testHomography(pointsInImage: MatOfPoint) {
    val questions = 35
    val H = ImageProcessing.findHomography(questions)(pointsInImage)

    val pointsInImage_mat = Converters.vector_Point2f_to_Mat(pointsInImage.toList)
    val dstPoints_mat = new Mat


    Core.perspectiveTransform(pointsInImage_mat, dstPoints_mat, H)

    val dstPoints = new util.ArrayList[Point]
    Converters.Mat_to_vector_Point2f(dstPoints_mat, dstPoints)

    //for (p <- dstPoints) {  println(p)  }

    val answerMatrixContour = AnswerMatrixMeasures.destinationContour(questions)

    assert(dstPoints.get(0) ~= answerMatrixContour.toArray()(0))
    assert(dstPoints.get(1) ~= answerMatrixContour.toArray()(1))
    assert(dstPoints.get(2) ~= answerMatrixContour.toArray()(2))
    assert(dstPoints.get(3) ~= answerMatrixContour.toArray()(3))
  }
}

