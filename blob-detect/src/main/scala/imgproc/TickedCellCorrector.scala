package imgproc

import imgproc.ocr.{CrossRecognizer, DefaultCrossRecognizer, DefaultTrainedOneLetterOCR, OneLetterOCR}
import imgproc.ocr.OneLetterOCR.LetterResult
import org.opencv.core.{Mat, Rect}

import scala.collection.immutable.IndexedSeq

trait CellCorrector {
  def recognize(cell: Mat): String
}


class LetterCellCorrector(possibleAnswers: Int) extends CellCorrector{
  override def recognize(cell: Mat): String = {

    val e = OneLetterOCR.extractPossibleLettersImage(cell)
    val predictions = if( e.size == 0 ){
      Seq(DefaultTrainedOneLetterOCR.predict(cell))
    }
    else {
      e.map(DefaultTrainedOneLetterOCR.predict)
    }
    predictions.filter(_.prediction.isDefined).map(_.prediction.get).mkString(",")

  }
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
      ImageProcessing.submatrix(cell, new Rect(w*i,0,w,h))
    }

    val results = subcells.map( recognizer.predict )
    results.filter(_.prediction.isDefined).
      map(_.prediction.get).
      mkString(",")
  }
}
