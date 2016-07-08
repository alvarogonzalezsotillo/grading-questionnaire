package imgproc

/**
  * Created by alvaro on 8/07/15.
  */

import org.junit.runner.RunWith
import org.opencv.core.Point
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class AnswerMatrixMeasuresTest extends FlatSpec {


  "AnswerMatrixMeasuresHorizontalTicked" should "have the last cell in the bottom right corner" in {

    val ammht = new AnswerMatrixMeasuresHorizontalTicked
    val p = 50
    val r = ammht.answerTableRect(p)
    val lastCell = ammht.answerCells(p).last

    import ammht.Params._
    val freeWidthAfterCell = answerCellAvailableWidth - cellHeaderToCellWidthGap - cellSize.w


    assert(lastCell.corners(2) + freeWidthAfterCell.toPoint == r.corners(2))

  }

  "AnswerMatrixMeasuresHorizontalTicked" should "have the fifth cell in the upper right corner" in {

    val ammht = new AnswerMatrixMeasuresHorizontalTicked
    val p = 50
    val r = ammht.answerTableRect(p)
    val lastCell = ammht.answerCells(p)(4)

    import ammht.Params._
    val freeWidthAfterCell = answerCellAvailableWidth - cellHeaderToCellWidthGap - cellSize.w

    assert(lastCell.corners(1) + freeWidthAfterCell.toPoint == r.corners(1))
  }

}
