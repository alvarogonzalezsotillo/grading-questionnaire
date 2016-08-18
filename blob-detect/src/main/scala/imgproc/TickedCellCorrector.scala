package imgproc

import imgproc.ocr.{CrossRecognizer, DefaultCrossRecognizer}
import imgproc.ocr.OneLetterOCR.LetterResult
import org.opencv.core.{Mat, Rect}

import scala.collection.immutable.IndexedSeq

trait CellCorrector {
  def recognize(cell: Mat): String
}

/**
  * Created by alvaro on 10/08/16.
  */
class TickedCellCorrector(possibleAnswers: Int) extends CellCorrector{

  private val recognizer = DefaultCrossRecognizer

  override def recognize(cell: Mat): String = {
    val w = cell.width()/possibleAnswers
    val h = cell.height()
    val subcells = for( i <- 0 until possibleAnswers ) yield{
      ImageProcessing.submatrix(cell, new Rect(w*i,0,w*(i+1),h))
    }

    val results = subcells.map( recognizer.predict )
    results.filter(_.prediction.isDefined).
      map(_.prediction.get).
      mkString(",")
  }
}
