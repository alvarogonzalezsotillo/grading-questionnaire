package imgproc

import java.io.File

import org.opencv.core.Mat
import org.opencv.highgui.Highgui

/**
 * Created by alvaro on 15/03/16.
 */
object TestUtil {


  private val testImgPath = {

    def remove(f: File): Unit = if (f.isFile) {
      f.delete()

    }
    else {
      val files = f.listFiles
      if (files != null) {
        files.foreach(remove)
      }
    }

    val p = new File("./build/test-img")
    remove(p)
    p.mkdir()
    p
  }

  private def testImgPath(file: String): File = new File(testImgPath, file)

  val positiveMatchImages = Seq(
    "2016-01-26-101322.jpg",
    "2016-01-26-101343.jpg",
    "2016-01-26-101403.jpg",
    "2016-01-26-101423.jpg",
    "2016-01-26-101448.jpg",
    "2016-01-26-101502.jpg",
    "2016-01-26-101516.jpg"
  )

  def saveTestImage(name: String, m: Mat) = {
    println( s"saving ${testImgPath(name).getAbsolutePath}")
    Highgui.imwrite(testImgPath(name).toString, m)
  }


}
