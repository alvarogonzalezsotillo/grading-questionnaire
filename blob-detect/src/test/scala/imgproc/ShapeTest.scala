package imgproc

import org.junit.runner.RunWith
import org.opencv.core.{MatOfPoint, Point}
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import java.lang.Math._

@RunWith(classOf[JUnitRunner])
class ShapeTest extends FlatSpec {

  import imgproc.Implicits._

  imgproc.OpenCVLib.loadLibrary()

  behavior of "Shape"

  it should "grows" in {
    val s = new MatOfPoint( (0.0, 0.0), (0.0, 1.0), (1.0, 1.0), (1.0, 0.0))
    val grown: MatOfPoint = s.grow(2)
    println( grown.points.mkString(",") )
    println( (-sqrt(2)).toInt )
    assert(grown.points(0).x ~= (-sqrt(2)).toInt )


  }
}