package imgproc

import java.io.{PrintStream, File}
import javax.imageio.ImageIO
import org.opencv.core.Mat
import org.opencv.highgui.Highgui

import scala.util.Try

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
    import imgproc.Implicits._
    //println( s"saving ${testImgPath(name).getAbsolutePath}")
    // IT IS NOT A GOOD IDEA TO USE OPENCV, SINCE THE VERSION OF LIBPNG SHOULD MATCH
    //Highgui.imwrite(testImgPath(name).toString, m)
    val format = name.takeRight(3).toLowerCase
    assert( name.takeRight(4).head == '.' )
    val f = testImgPath(name)
    f.getParentFile.mkdirs()
    ImageIO.write(m,format, f)

  }

  case class SomeTestsResult( allowedFailureRatio: Double, total: Int, failures: Int )

  def runSomeTestAndFailIfSoMuchFailures[I,T](files: Seq[I], showFailures : Boolean = false, allowedFailureRatio: Double = 0.2)(test: I => T): SomeTestsResult = {

    def runSomeTestAndCollectFailures[T](files: Seq[I])(test: I => T) = {
      val results = for (f <- files) yield (f, Try(test(f)))
      results.filter(_._2.isFailure)
    }

    def reportFailures[T](failures: Seq[(I, Try[T])], out: PrintStream = System.out) {
      for ((file, failure) <- failures) {
        out.println(file)
        failure.failed.get.printStackTrace(out)
      }
    }

    val failures = runSomeTestAndCollectFailures(files)(test)
    if (showFailures) reportFailures(failures)
    val ret = SomeTestsResult(allowedFailureRatio, files.size, failures.size )
    assert(failures.size < allowedFailureRatio * files.size, s"To much failures: $ret")
    ret
  }

  def ignored( proc : => Unit) = {}



}
