import org.opencv.core.{Size, CvType, Mat, MatOfPoint,Scalar,Core}
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 29/10/15.
 */
object ImageProcessing {

  def threshold(blockSize:Int=101,C:Double=3)(src: Mat) : Mat = {
    val dst = new Mat(src.height(),src.width(),CvType.CV_8UC1)
    Imgproc.cvtColor(src,dst,Imgproc.COLOR_RGB2GRAY)
    Imgproc.adaptiveThreshold( dst, dst, 255, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY,blockSize,C)
    dst
  }

  def clean(sizeOpen:Int=18, sizeClose:Int=7)(src: Mat): Mat = {
    val open = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(sizeOpen, sizeOpen))
    val close = Imgproc.getStructuringElement(Imgproc.MORPH_RECT, new Size(sizeClose, sizeClose))
    try {
      Imgproc.morphologyEx(src, src, Imgproc.MORPH_CLOSE, close)
      Imgproc.morphologyEx(src, src, Imgproc.MORPH_OPEN, open)
    }
    catch {
      case t: Throwable => t.printStackTrace()
    }
    src
  }

  def findContours(rgbaImage: Mat ) : Seq[MatOfPoint]= {
      import java.util.ArrayList;
      import scala.collection.JavaConversions._
      
      val mPyrDownMat = new Mat
        Imgproc.pyrDown(rgbaImage, mPyrDownMat);
        Imgproc.pyrDown(mPyrDownMat, mPyrDownMat);


        val contours = new ArrayList[MatOfPoint]();
        val mHierarchy = new Mat
        Imgproc.findContours(mPyrDownMat, contours, mHierarchy, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);

        val scale = new Scalar(4,4)        
        contours.map{ c => Core.multiply(c, scale, c);c }
  }
      
   
}
