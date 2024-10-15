package imgproc

import java.awt.AlphaComposite
import java.awt.image.{BufferedImage, DataBufferByte}

import org.opencv.core._
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 19/10/15.
 */
object Implicits {

  implicit def Mat2BufferedImage(m:Mat): BufferedImage = {
    // source: http://answers.opencv.org/question/10344/opencv-java-load-image-to-gui/
    // Fastest code
    // The output can be assigned either to a BufferedImage or to an Image

    if( m == null ){
      return ImageCanvas.defaultImage
    }

    val imageType = if ( m.channels() > 1 )
      BufferedImage.TYPE_3BYTE_BGR
    else
      BufferedImage.TYPE_BYTE_GRAY

    val bufferSize = m.channels()*m.cols()*m.rows()
    val b = new Array[Byte](bufferSize)
    m.get(0,0,b); // get all the pixels
    val image = new BufferedImage(m.cols(),m.rows(), imageType )
    val targetPixels = (image.getRaster().getDataBuffer()).asInstanceOf[DataBufferByte].getData()
    System.arraycopy(b, 0, targetPixels, 0, b.length)

    image
  }




  def toBufferedImageOfType(original:BufferedImage, typeI: Int) = {

    // Don't convert if it already has correct type
    if (original.getType() == typeI) {
      original
    }
    else {
      // Create a buffered image
      val image = new BufferedImage(original.getWidth(), original.getHeight(), typeI)

      // Draw the image onto the new buffer
      val g = image.createGraphics();
      try {
        g.setComposite(AlphaComposite.Src);
        g.drawImage(original, 0, 0, null);
      }
      finally {
        g.dispose()
      }

      image
    }
  }

  implicit def BufferedImage2Mat( image: BufferedImage ) : Mat = {
    val ret = new Mat(image.getHeight,image.getWidth,CvType.CV_8UC3)
    BufferedImage2Mat(image, ret)
    ret
  }

  def BufferedImage2Mat( src: BufferedImage, dst: Mat ) : Unit {
    val image = toBufferedImageOfType(src,BufferedImage.TYPE_3BYTE_BGR)
    try {
      assert(image.getType == BufferedImage.TYPE_3BYTE_BGR)
      assert(dst.`type` == CvType.CV_8UC3)
      assert(dst.width == image.getWidth)
      assert(dst.height == image.getHeight)
      val pixels = (image.getRaster().getDataBuffer()).asInstanceOf[DataBufferByte].getData()
      dst.put(0, 0, pixels)
    }
    catch{
      case t : Throwable => t.printStackTrace()
    }
  }

  implicit class MyMat(m:Mat){
    lazy val rect = new Rect(0,0,m.width(),m.height())
    lazy val area = rect.area()
    lazy val toIntArray = {
      val ret = new Array[Array[Int]](m.rows())
      for( r <- 0 until m.rows ){
        ret(r) = new Array[Int](m.cols())
        for( c <- 0 until m.cols ){
          ret(r)(c) = m.get(r,c)(0).toInt
        }
      }
      ret
    }
  }

  implicit def toMatOfPoint( points: Seq[Point] ) = new MatOfPoint(points:_*)

  implicit class Shape(contour: MatOfPoint){
    lazy val points = contour.toArray
    def apply(index:Int) = points(index)
    lazy val center = {
      val c = points.tail.foldLeft( points(0) ) {
        (p, center) => p+center
      }
      c / points.size
    }

    lazy val boundingBox = {
      val minX = points.map(_.x).min.toInt
      val minY = points.map(_.y).min.toInt
      val maxX = points.map(_.x).max.toInt
      val maxY = points.map(_.y).max.toInt
      new Rect(minX,minY,maxX-minX,maxY-minY)
    }

    def grow( inc: Double ) : MatOfPoint = {
      val c = center
      //println( s"Center:$center")
      val newPoints = points.map {p =>
        //println( s"  p:$p")
        val v = p - c
        //println( s"  v:$v")
        val modulus = v.modulus + inc
        //println( s"  modulus:$modulus")

        val ret = c + v.withModulus( modulus )
        //println( s"  ret:$ret")
        //println()
        ret
      }

      //println( s"  grow: ${newPoints.mkString(",")}")
      new MatOfPoint(newPoints :_* )
    }

    lazy val area = Imgproc.contourArea(contour)
  }

  implicit def toMat( array: Array[Array[Int]] ) = {
    val rows = array.size
    val cols = array(0).size
    val ret = new Mat(rows,cols,CvType.CV_8UC1)
    val buffer = new Array[Byte](1)
    for( r <- 0 until rows ; c <- 0 until cols ){
      buffer(0) = array(r)(c).toByte
      ret.put(r,c,buffer)
    }
    ret
  }

  implicit class MyRect(rect: Rect){
    val minX = rect.x
    val minY = rect.y
    val maxX = rect.x + rect.width
    val maxY = rect.y + rect.height

    def union( r: Rect ) = {
      val xmin = minX min r.minX
      val ymin = minY min r.minY
      val xmax = maxX max r.maxX
      val ymax = maxY max r.maxY
      new Rect( xmin, ymin, xmax-xmin, ymax-ymin)
    }

    def grow( n: Int ) = new Rect( rect.x-n, rect.y-n, rect.width+2*n, rect.height+2*n)

    def intersection( r: Rect ) : Option[Rect]= {
      if( !overlaps(r) ){
        None
      }
      else{
        val xmin = minX max r.minX
        val ymin = minY max r.minY
        val xmax = maxX min r.maxX
        val ymax = maxY min r.maxY
        Some(new Rect( xmin, ymin, xmax-xmin, ymax-ymin))
      }
    }

    def overlaps( r: Rect ) = {
      val ox = (minX max r.minX) - (maxX min r.maxX)
      val oy = (minY max r.minY) - (maxY min r.maxY)
      ox <= 0 && oy <= 0
    }

    lazy val asShape = new MatOfPoint( new Point(minX,minY), new Point(maxX,minY), new Point(maxX,maxY), new Point(minX,maxY) )
  }

  implicit class RubyPostfixConditionals[T]( proc: => T ){
    def If( b: Boolean) = if(b) Some(proc) else None
    def If( condition: T => Boolean) = {
      val ret = proc
      if( condition(ret) ) Some(ret) else None
    }

    def Unless(b: Boolean ) = If(!b)
  }



  implicit class MyPoint(val point:Point){
    def dotProduct(p:Point) : Double = point.x*p.x + point.y*p.y

    def product(d:Double) = new Point(point.x*d,point.y*d)

    def crossProductZ(p:Point) : Double = point.x*p.y - point.y*p.x

    def modulus = Math.sqrt( dotProduct(this) )

    def normalize = this * (1/modulus)

    def withModulus( m: Double ) = normalize * m

    def minus(p:Point) = new Point(point.x-p.x,point.y-p.y)
    def plus(p:Point) = new Point(point.x+p.x,point.y+p.y)

    def *(p:Point) : Double = dotProduct(p)
    def *(d:Double) = product(d)
    def /(d:Double) = product(1/d)
    def -(p:Point) = minus( p )
    def +(p:Point) = plus( p )

    def ~=(p:Point)(implicit e: Epsilon) = (point.x ~= p.x) && (point.y ~= p.y)

    def distanceToLine( p1: Point, p2: Point ) : Double = {
      // https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
      val v = (p1-p2)
      val lambda = (this * v - p2*v )/(v*v)
      val projectionOfThisOnP1P2 = v*lambda + p2
      (this - projectionOfThisOnP1P2).modulus
    }

  }

  implicit def toOpenCVPoint(p:MyPoint) : Point = p.point

  implicit def toOpenCVPoint( p:(Double,Double) ) : Point = new Point(p._1,p._2)

  case class Epsilon(epsilon:Double)

  implicit class DoubleComparator(val value:Double){
    def ~=(d:Double)(implicit epsilon: Epsilon) = Math.abs(this.value - d) < epsilon.epsilon
  }

  implicit def rectToMatOfPoint( r: Rect ) : MatOfPoint = r.asShape
}
