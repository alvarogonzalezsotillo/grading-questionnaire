package imgproc.ocr

import org.opencv.core.Mat

/**
 * Created by alvaro on 20/01/16.
 */
object OneLetterOCR {

  case class LetterProb(char: Char, probability: Double) {
    assert(probability >= 0 && probability <= 1)
  }
  
  type LetterResult = Seq[LetterProb]

  def scan( m: Mat ) : LetterResult = Seq( LetterProb('a',1) )

  def apply( m: Mat ) = scan(m)

}
