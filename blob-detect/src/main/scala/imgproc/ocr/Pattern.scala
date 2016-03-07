package imgproc.ocr

import imgproc.ImageProcessing
import org.opencv.core.Mat

/**
 * Created by alvaro on 3/02/16.
 */

trait Pattern{
  val letter: Char
  val mats: Seq[Mat]
}

object Pattern{

  val patternSize: Int = 20


  lazy val patterns: Seq[Pattern] = {

    class PatternI( val letter: Char, val mats: Seq[Mat] ) extends Pattern


    def loadPattern( letter: Char, prefix: String, index: Int ) = {
      /*
      val fileName = String.format( "/ocr/patterns/%s-%c-%03d.png", prefix, letter.toLower, index )
      ImageProcessing.readImageFromResources(fileName)
      */
      ???
    }

    def loadPatterns( letters: Seq[Char], prefix: String ) ={
      for( l <- letters ) yield {
        val patterns = (0 to Int.MaxValue).
          map(i => loadPattern(l,prefix,i) ).
          takeWhile( _ != null ).
          map( m => ImageProcessing.stretchImage()(m,patternSize,patternSize) )
        new PatternI( l, patterns )
      }
    }

    val letters = ('a' to 'd')
    val lowerCaseLetters = loadPatterns( letters.map(_.toLower), "lower" )
    val upperCaseLetters = loadPatterns( letters.map(_.toUpper), "upper" )

    lowerCaseLetters ++ upperCaseLetters
  }

}
