package imgproc

import imgproc.ocr.Pattern
import org.opencv.calib3d.Calib3d
import org.opencv.core._
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 29/10/15.
 */
object ImageProcessing {

  import imgproc.Implicits._


  def readImageFromResources[T](f: String, clazz: java.lang.Class[T] = ImageProcessing.getClass ): Mat = {
    def mat() = {
      val url = clazz.
        getResource(f).
        getPath
      
      Highgui.imread(url)
    }

    def imageio() : Mat = {
      val url = getClass().getResource(f)
      if( url != null ) javax.imageio.ImageIO.read(url) else null
    }

    imageio
  }

  def toGrayscaleImage( src: Mat ) = {
    val dst = new Mat(src.height(), src.width(), CvType.CV_8UC1)
    Imgproc.cvtColor(src, dst, Imgproc.COLOR_RGB2GRAY)
    dst
  }

  def threshold(blockSize: Int = 101, C: Double = 3)(src: Mat): Mat = {
    val dst = toGrayscaleImage(src)
    Imgproc.adaptiveThreshold(dst, dst, 255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY_INV, blockSize, C)
    dst
  }



  def toColorImage(src: Mat) = {
    if (src.channels() > 1)
      src
    else {
      val dst = new Mat
      Imgproc.cvtColor(src, dst, Imgproc.COLOR_GRAY2RGB)
      dst
    }
  }


  def canny(t1: Int = 1, t2: Int = 20)(m: Mat) = {
    val ret = new Mat
    Imgproc.Canny(m, ret, t1, t2)
    ret
  }

  def meanShift(spatialWindow : Int = 2, colorWindow:  Int = 20)(m: Mat) = {
    val ret = new Mat
    Imgproc.pyrMeanShiftFiltering(m, ret, spatialWindow, colorWindow)
    ret
  }


  private def newMatrixIfNull(m:Mat) = if( m == null ) new Mat else m

  def clean(iterations: Int = 3, sizeOpen: Int = 7, sizeClose: Int = 7)(dst: Mat = null)(src: Mat): Mat = {

    val ret = newMatrixIfNull(dst)
    val open = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(sizeOpen, sizeOpen))
    val close = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(sizeClose, sizeClose))
    try {
      // TODO: HOW TO USE ITERATIONS AND ANCHOR TO NOT DISPLACE RESULTS? MEANWHILE, EXTERNAL LOOP
      var currentMat = src
      //val anchorClose = new Point(1.0 * sizeClose / 2, 1.0 * sizeClose / 2)

      for (i <- 1 to iterations) {
        Imgproc.morphologyEx(currentMat, ret, Imgproc.MORPH_CLOSE, close)
        currentMat = ret
      }
      //val anchorOpen = new Point(1.0 * sizeOpen / 2, 1.0 * sizeOpen / 2)
      for (i <- 1 to iterations) {
        Imgproc.morphologyEx(ret, ret, Imgproc.MORPH_OPEN, open)
      }
    }
    catch {
      case t: Throwable => t.printStackTrace()
    }
    ret
  }

  def findContours(m: Mat): Seq[MatOfPoint] = {
    import java.util.ArrayList

    import scala.collection.JavaConversions._

    /*
    val mPyrDownMat = new Mat
    Imgproc.pyrDown(m, mPyrDownMat)
    Imgproc.pyrDown(mPyrDownMat, mPyrDownMat)
    val scale = new Scalar(4, 4)
    */
    val mPyrDownMat = m
    val scale = new Scalar(1, 1)

    val contours = new ArrayList[MatOfPoint]()
    val mHierarchy = new Mat
    Imgproc.findContours(mPyrDownMat, contours, mHierarchy, Imgproc.RETR_LIST, Imgproc.CHAIN_APPROX_TC89_KCOS)
    contours.map { c => Core.multiply(c, scale, c); c}
  }

  def approximateContoursToQuadrilaterals(epsilon: Double = 10)(contours: Seq[MatOfPoint]): Seq[MatOfPoint] = {
    contours.map(convertToQuadrilateral(epsilon)).filter(_.isDefined).map(_.get)
  }

  def convertToQuadrilateral(epsilon: Double)(contour: MatOfPoint): Option[MatOfPoint] = {

    val contour2f = new MatOfPoint2f()
    val approxContour = new MatOfPoint()
    val approxContour2f = new MatOfPoint2f()

    contour.convertTo(contour2f, CvType.CV_32FC2)

    Imgproc.approxPolyDP(contour2f, approxContour2f, epsilon, true)

    approxContour2f.convertTo(approxContour, CvType.CV_32S)

    val edges = approxContour.size().height
    val min = 4
    val max = 4
    if (edges >= min && edges <= max) {
      Some(approxContour)
    }
    else {
      None
    }
  }

  private val defaultColor = new Scalar(0,255,0)

  def drawContours(dst: Mat, contours: Seq[MatOfPoint], color: Scalar = defaultColor, thickness: Int = 3, drawCenters: Boolean = false): Mat = {
    import scala.collection.JavaConversions._
    Imgproc.drawContours(dst, contours, -1, color, thickness)
    Imgproc.drawContours(dst, contours.map(c => new MatOfPoint(c.center)), -1, color, thickness) If drawCenters
    dst
  }



  def drawString(dst: Mat, string: String, point: Point, color: Scalar = defaultColor): Mat = {
    val fontFace = Core.FONT_HERSHEY_SIMPLEX
    val fontScale = 0.6
    Core.putText(dst,string,point,fontFace,fontScale,color)
    dst
  }

  def findBiggestAlignedQuadrilaterals(number: Int = 5)(contours: Seq[MatOfPoint]): Option[Seq[MatOfPoint]] = {
    val ordered = contours.sortBy(_.area).reverse.take(number)
    
    def similarQuadrilaterals(quad: MatOfPoint) = {
      implicit val epsilon = Epsilon(quad.area*0.10)
      contours.filter(_.area ~= quad.area )
    }

    ordered.view.map(similarQuadrilaterals).filter(_.size==number).headOption
  }



  def findHomography(srcPoints: MatOfPoint, dstPoints : MatOfPoint) = {
    val dstPoints2f = new MatOfPoint2f()
    val srcPoints2f = new MatOfPoint2f()
    srcPoints.convertTo(srcPoints2f, CvType.CV_32FC2)
    dstPoints.convertTo(dstPoints2f, CvType.CV_32FC2)
    Calib3d.findHomography(srcPoints2f, dstPoints2f)
  }

  def warpImage(dst: Mat = null)(m: Mat, H: Mat, size: Size = null) = {
    val ret = newMatrixIfNull(dst)
    val s = if (size == null) m.size() else size
    Imgproc.warpPerspective(m, ret, H, s)
    ret
  }

  def submatrix(m:Mat, rect: Rect) : Mat = {
    new Mat(m,rect).clone()
  }

  def stretchImage(dst: Mat = null)(m:Mat, w:Int, h:Int) = {
    val ret = newMatrixIfNull(dst)
    Imgproc.resize(m, ret, new Size(w, h))
    ret
  }

}
