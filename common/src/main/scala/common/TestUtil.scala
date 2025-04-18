package common

import java.awt.Image
import java.awt.image.RenderedImage
import java.io.{File, PrintStream}
import javax.imageio.ImageIO

import scala.util.Try
import org.opencv
import org.opencv.core.Mat

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

  def removeFileExtension(s: String) = {
    val file = new File(s)
    file.getName.reverse.dropWhile(_ != '.').tail.reverse
  }



  def saveTestImage(name: String, image: RenderedImage) = {
    //println( s"saving ${testImgPath(name).getAbsolutePath}")
    // IT IS NOT A GOOD IDEA TO USE OPENCV, SINCE THE VERSION OF LIBPNG SHOULD MATCH
    //Highgui.imwrite(testImgPath(name).toString, m)
    val format = name.takeRight(3).toLowerCase
    assert( name.takeRight(4).head == '.' )
    val f = testImgPath(name)
    f.getParentFile.mkdirs()

    ImageIO.write(image,format, f)

  }


  def saveDerivedTestImage(imageLocation: String, stepName: String, m: RenderedImage, group: String = "processing-step" ) = {
    saveTestImage(group + "/" + removeFileExtension(imageLocation) + "/" + stepName + ".png", m)
  }


  case class SomeTestsResult( allowedFailureRatio: Double, total: Int, failures: Int )

  def runSomeTestAndFailIfSoMuchFailures[I,T](files: Seq[I], showFailures : Boolean = false, allowedFailureRatio: Double = 0.2)(test: I => T): SomeTestsResult = {

    def runSomeTestAndCollectFailures[T](files: Seq[I])(test: I => T) = {
      val results = for (f <- files) yield (f, Try(test(f)))
      results.filter(_._2.isFailure)
    }

    def reportFailures[T](failures: Seq[(I, Try[T])], out: PrintStream = System.out) : Unit = {
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
