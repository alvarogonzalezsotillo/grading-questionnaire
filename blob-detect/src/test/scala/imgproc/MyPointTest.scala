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

  "Distance to line" should "be 0 if on line" in{
    val p1 = new Point(1,1)
    val p2 = new Point(2,2)

    val p = new Point(1.5,1.5)

    val distance = p.distanceToLine(p1,p2)

    println( s"Distance:$distance")

    assert(distance ~= 0)
  }


  "Distance to line" should "work with the points of a square" in{
    val p1 = new Point(1,1)
    val p2 = new Point(2,2)

    val p = new Point(1,2)

    val distance = p.distanceToLine(p1,p2)

    println( s"Distance:$distance")

    assert(distance ~= Math.sqrt(2)/2)
  }



}

