package imgproc

/**
 * Created by alvaro on 8/07/15.
 */

import org.junit.runner.RunWith
import org.opencv.core.Point
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class MyPointTest extends FlatSpec {

  import imgproc.Implicits._

  case class Epsilon(epsilon:Double)

  implicit class DoubleComparator(val value:Double){
    def ~=(d:Double)(implicit epsilon: Epsilon) = Math.abs(this.value - d) < epsilon.epsilon

  }

  implicit val epsilon = Epsilon(0.01)

  "Dot product" should "be zero if perpendicular" in {

    val p1 = new Point(1,1)
    val p2 = new Point(1,-1)

    assert( p1 * p2 == 0 )
  }

  "Dot product" should "be like modulus if parallel" in{
    val p1 = new Point(5,1)
    val p2 = new Point(-10,-2)

    assert( Math.abs(p1 * p2) ~= p1.modulus * p2.modulus )
  }

  "Cross product" should "be zero if parallel" in {
    val p1 = new Point(5,1)
    val p2 = new Point(-10,-2)

    assert( p1.crossProductZ(p2) == 0 )
  }
  
  "Cross product" should "be like modulus if perpendicular" in{
    val p1 = new Point(4,4)
    val p2 = new Point(6,-6)

    assert( Math.abs(p1.crossProductZ(p2)) ~= p1.modulus*p2.modulus )
  }





}

