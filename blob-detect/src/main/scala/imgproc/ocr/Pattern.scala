package imgproc.ocr

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import com.typesafe.scalalogging.slf4j.LazyLogging
import imgproc.ImageProcessing
import org.opencv.core.Mat
import imgproc.Implicits._
import org.opencv.imgproc.Imgproc

/**
 * Created by alvaro on 3/02/16.
 */

object Pattern extends LazyLogging{

  val patternSize: Int = 20



  def resizeToPatterSize(m: Mat) = ImageProcessing.stretchImage()(m,patternSize,patternSize)

  type TrainingPatterns = Map[Char,Seq[Mat]]

  private val baseDirs = Seq(
    new File("blob-detect/src/main/resources/training-models/"),
    new File("src/main/resources/training-models/")
  )

  private def loadImagesFromDir( dir: String ) = {
    val a = for( baseDir <- baseDirs ) yield {
      val f = new File(baseDir, dir)
      val files = f.listFiles()
      if( files != null ) files.map(ImageIO.read) else null
    }

    val b = a.filter(_!=null)

    val c = b.flatten

    c
  }


  lazy val letterTrainingPatterns:  TrainingPatterns = {

    val maxPatterns = 50;

    def loadPatterns( letters: Seq[Char]  ) = {

      def loadPatternsFromLetter( letter: Char ) = loadImagesFromDir(s"uppercase-${letter.toLower}" ).take(maxPatterns)

      val pairs = for( l <- letters ) yield{
        l -> loadPatternsFromLetter(l).map(m => resizeToPatterSize(m)).toSeq
      }
      //val unrecognizables = loadImagesFromDir( "unrecognizable" ).map(m => resizeToPatterSize(m))
      //Map( pairs :_* ) ++ Map( '@' -> unrecognizables )
      Map( pairs :_* )
    }


    loadPatterns( 'A' to 'D' )
  }

  lazy val crossTrainingPatterns : TrainingPatterns = {
    def loadPatternsFromSheet(dir:String) = {
      loadImagesFromDir(dir).
        map(BufferedImage2Mat).
        flatMap(OneLetterOCR.extractPossibleLettersImage(_)).
        map(resizeToPatterSize)
    }

    val crosses = loadPatternsFromSheet("cross")

    val invalidCrosses = loadPatternsFromSheet("invalid-cross")


    Map( 'X' -> crosses, 'O' -> invalidCrosses )
  }

  lazy val emptyPatterns : TrainingPatterns = {
    val empties = loadImagesFromDir("empty").map(BufferedImage2Mat).map(resizeToPatterSize)
    Map( ' ' -> empties )
  }

}
