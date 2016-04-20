package imgproc.ocr

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import imgproc.ImageProcessing
import org.opencv.core.Mat
import imgproc.Implicits._

/**
 * Created by alvaro on 3/02/16.
 */

object Pattern{

  val patternSize: Int = 20

  def resizeToPatterSize(m: Mat) = ImageProcessing.stretchImage()(m,patternSize,patternSize)

  type TrainingPatterns = Map[Char,Seq[Mat]]

  lazy val trainingPatterns:  TrainingPatterns = {

    val baseDirs = Seq(
      new File("blob-detect/src/main/resources/training-models/"),
      new File("src/main/resources/training-models/")
    )



    def loadPatternsFromLetter( letter: Char ) = loadPatternsFromDir(s"uppercase-${letter.toLower}" )

    def loadPatternsFromDir( dir: String ) = {
      val a = for( baseDir <- baseDirs ) yield {
        val f = new File(baseDir, dir)
        val files = f.listFiles()
        if( files != null ) files.map(ImageIO.read) else null
      }

      val b = a.filter(_!=null)

      val c = b.flatten

      c
    }



    def loadPatterns( letters: Seq[Char]  ) = {
      val pairs = for( l <- letters ) yield{
        l -> loadPatternsFromLetter(l).map(m => resizeToPatterSize(m)).toSeq
      }
      val unrecognizables = loadPatternsFromDir( "unrecognizable" ).map(m => resizeToPatterSize(m))
      Map( pairs :_* ) ++ Map( '@' -> unrecognizables )
    }


    loadPatterns( 'A' to 'D' )
  }

}
