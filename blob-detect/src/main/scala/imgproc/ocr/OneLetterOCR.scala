package imgproc.ocr

import imgproc.ImageProcessing
import org.opencv.core.{Core, Mat}
import org.opencv.imgproc.Imgproc

import scala.collection.JavaConverters._

/**
 * Created by alvaro on 20/01/16.
 */
object OneLetterOCR {

  case class LetterProb(char: Char, probability: Double) {
    assert(probability >= 0 && probability <= 1)
  }
  
  type LetterResult = Seq[LetterProb]

  def average[T]( ts: Iterable[T] )( implicit num: Numeric[T] ) = {
    num.toDouble( ts.sum ) / ts.size
  }


  def extractPossibleLetters( m: Mat ) = Seq(m)

  def normalizeLetter(mat: Mat) : Mat = ???

  def scan( m: Mat ) : LetterResult = {

    val normalized = normalizeLetter(m)

    def howMuchDifferent(m: Mat): Double = {
      ???
    }

    for (p <- Pattern.patterns) yield {
      val differences = p.mats.map(howMuchDifferent)
      val minDifference = differences.max
      val avgDifference = average(differences)
      val probability = (1.0 / minDifference) max 1
      new LetterProb(p.letter, probability)
    }

  }

  def apply( m: Mat ) = scan(m)

}
