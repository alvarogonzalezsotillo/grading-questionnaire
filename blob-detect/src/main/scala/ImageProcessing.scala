import org.opencv.core._
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 29/10/15.
 */
object ImageProcessing {

  def threshold(blockSize: Int = 101, C: Double = 3)(src: Mat): Mat = {
    val dst = new Mat(src.height(), src.width(), CvType.CV_8UC1)
    Imgproc.cvtColor(src, dst, Imgproc.COLOR_RGB2GRAY)
    Imgproc.adaptiveThreshold(dst, dst, 255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY, blockSize, C)
    dst
  }

  def clean(sizeOpen: Int = 18, sizeClose: Int = 7)(dst: Mat = null)(src: Mat): Mat = {

    val ret = if (dst == null) {
      new Mat
    }
    else {
      dst
    }
    val open = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(sizeOpen, sizeOpen))
    val close = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(sizeClose, sizeClose))
    try {
      Imgproc.morphologyEx(src, ret, Imgproc.MORPH_CLOSE, close)
      Imgproc.morphologyEx(ret, ret, Imgproc.MORPH_OPEN, open)
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

  def drawContours(dst: Mat, contours: Seq[MatOfPoint], color: Scalar, thickness: Int = 1): Mat = {
    import scala.collection.JavaConversions._
    Imgproc.drawContours(dst, contours, -1, color,thickness)
    dst
  }

  //def groupContoursByArea


}
