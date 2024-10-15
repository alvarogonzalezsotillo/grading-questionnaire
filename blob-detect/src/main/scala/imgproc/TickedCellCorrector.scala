package imgproc

import common.TestUtil
import imgproc.TickedCellCorrector.seq
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

object TickedCellCorrector{
  object seq extends (()=>Int) {
    private var last = 0
    def apply() = {
      val ret = last+1
      last += 1
      ret
    }
  }
}

class TickedCellCorrector(possibleAnswers: Int) extends CellCorrector{

  private val recognizer = DefaultCrossRecognizer

  override def recognize(cell: Mat): String = {
    val subcells = {
      val w = cell.width()/possibleAnswers
      val h = cell.height()
      (0 until possibleAnswers ).map( i => ImageProcessing.submatrix(cell, new Rect(w*i,0,w,h)) )
    }

    val trimmedSubcells = {
      val borderFactor = 0.15
      val bx = subcells(0).width() * borderFactor
      val by = subcells(0).height() * borderFactor
      val x = bx
      val y = by
      val w = subcells(0).width() - bx * 2
      val h = subcells(0).height() - by * 2

      subcells.map( sc => ImageProcessing.submatrix(sc, new Rect(x.toInt,y.toInt,w.toInt,h.toInt) ) )
    }

    if( true ){
      import imgproc.Implicits._
      val s = seq()
      for( i <- 0 until subcells.size ){
        val c = subcells(i)
        val tc = trimmedSubcells(i)
        val histogram = ImageProcessing.histogram(OneLetterOCR.normalizeLetter(tc),32).mkString("-")
        println( s"cell-$s-$i -> $histogram"   )
        TestUtil.saveTestImage( s"tickedCellCorrector/cell-$s-$i.png", c )
        TestUtil.saveTestImage( s"tickedCellCorrector/cell-$s-$i-trimmed-$histogram.png", tc )
        TestUtil.saveTestImage( s"tickedCellCorrector/cell-$s-$i-normalized.png", OneLetterOCR.normalizeLetter(tc) )
      }
    }

    val results = subcells.map( recognizer.predict )
    val resultsAsLetters = results.zipWithIndex.map{ case (r,i) =>
      if( r.significative && r.prediction.get == 'X' ) ('a' + i).toChar else ' '
    }

    resultsAsLetters.filter( _ != ' ').mkString(",")
  }
}
