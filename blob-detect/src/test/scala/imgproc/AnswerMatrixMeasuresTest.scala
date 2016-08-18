package imgproc

/**
  * Created by alvaro on 8/07/15.
  */

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class AnswerMatrixMeasuresTest extends FlatSpec {

  val questions = Some(25)
  val v = 1

  "The number of rows" should "be correct" in {


    assert(AnswerMatrixMeasures(Some(1),v).rows == 1)


    assert(AnswerMatrixMeasures(Some(5),v).rows == 1)
    assert(AnswerMatrixMeasures(Some(4),v).rows == 1)
    assert(AnswerMatrixMeasures(Some(6),v).rows == 2)


    assert(AnswerMatrixMeasures(Some(29),v).rows == 6)
    assert(AnswerMatrixMeasures(Some(30),v).rows == 6)
    assert(AnswerMatrixMeasures(Some(31),v).rows == 7)


  }

  "AnswerMatrixMeasuresHorizontalTicked" should "have the last cell in the bottom right corner" in {

    val ammht = AnswerMatrixMeasures(questions,v)
    val r = ammht.answerTableRect
    val lastCell = ammht.answerCells.last

    import ammht.params._
    val freeWidthAfterCell = answerCellAvailableWidth - cellHeaderToCellWidthGap - cellSize.w


    assert(lastCell.corners(2) + freeWidthAfterCell.toPoint == r.corners(2))

  }

  "AnswerMatrixMeasuresHorizontalTicked" should "have the fifth cell in the upper right corner" in {

    val ammht = AnswerMatrixMeasures(questions,v)
    val r = ammht.answerTableRect
    val lastCell = ammht.answerCells(4)

    import ammht.params._
    val freeWidthAfterCell = answerCellAvailableWidth - cellHeaderToCellWidthGap - cellSize.w

    assert(lastCell.corners(1) + freeWidthAfterCell.toPoint == r.corners(1))
  }

}
