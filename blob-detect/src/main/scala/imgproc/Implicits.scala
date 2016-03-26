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


  implicit def BufferedImage2Mat( image: BufferedImage ) : Mat = {
    val ret = new Mat(image.getHeight,image.getWidth,CvType.CV_8UC3)
    BufferedImage2Mat(image, ret)
    ret
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

  def BufferedImage2Mat( src: BufferedImage, dst: Mat ){
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
  }

  implicit class Shape(contour: MatOfPoint){
    lazy val points = contour.toArray
    lazy val center = {
      val c = points.foldLeft( new Point(0,0) ) { (p, center) =>
        center.x += p.x
        center.y += p.y
        center
      }
      c.x /= points.size
      c.y /= points.size
      c
    }

    lazy val boundingBox = {
      val minX = points.map(_.x).min.toInt
      val minY = points.map(_.y).min.toInt
      val maxX = points.map(_.x).max.toInt
      val maxY = points.map(_.y).max.toInt
      new Rect(minX,minY,maxX-minX,maxY-minY)
    }

    lazy val area = Imgproc.contourArea(contour)
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

    def overlaps( r: Rect ) = {
      println( s"overlaps $rect $r")
      println( s"  $minX $minY $maxX $maxY")
      println( s"  ${r.minX} ${r.minY} ${r.maxX} ${r.maxY}")
      val ox = (minX max r.minX) - (maxX min r.maxX)
      val oy = (minY max r.minY) - (maxY min r.maxY)
      println( s"  ox:$ox oy:$oy")
      val ret = ox <= 0 && oy <= 0
      println( s"  $ret" )
      ret
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
    def -(p:Point) = minus( p )
    def +(p:Point) = plus( p )

    def ~=(p:Point)(implicit e: Epsilon) = (point.x ~= p.x) && (point.y ~= p.y)

  }

  implicit def toOpenCVPoint(p:MyPoint) : Point = p.point

  implicit def toOpenCVPoint( p:(Double,Double) ) : Point = new Point(p._1,p._2)

  case class Epsilon(epsilon:Double)

  implicit class DoubleComparator(val value:Double){
    def ~=(d:Double)(implicit epsilon: Epsilon) = Math.abs(this.value - d) < epsilon.epsilon
  }

  implicit def rectToMatOfPoint( r: Rect ) : MatOfPoint = {
    new MatOfPoint( new Point(r.x,r.y), new Point(r.x+r.width, r.y), new Point(r.x+r.width, r.y + r.height), new Point(r.x,r.y+r.height) )
  }
}
