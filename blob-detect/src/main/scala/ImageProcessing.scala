import org.opencv.core.{Size, CvType, Mat}
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


}
