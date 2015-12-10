package imgproc

import org.opencv.core._

object AnswerMatrixMeasures {


  import imgproc.Implicits._

  val columns = 5
  val destinationWidth = 800.0
  val cellHeaderToHeaderWidthRatio = 0.7 / (0.7 + 1.3)
  val columnSpaceToCellWidthRatio = 0.15
  val matrixWithToTopOfQRRatio = 0.25
  val matrixWithToQRWidthRatio = 0.20


  val answerHeightRatio = 5.5 * columns
  val cellWidth = destinationWidth / (5 + 4 * columnSpaceToCellWidthRatio)
  val cellHeaderWidth = cellWidth * cellHeaderToHeaderWidthRatio
  val columnSpaceWidth = cellWidth * columnSpaceToCellWidthRatio

  // TODO: EXTRACT THIS FACTOR FROM OTHER FACTORS
  val extensionFactor = 1.6 / 12.6


  def rows(questions: Int) = (1.0 * questions / columns).ceil.toInt

  def destinationHeight(questions: Int) = {
    destinationWidth * rows(questions) / answerHeightRatio
  }


  def destinationContour(questions: Int) = {
    val w = AnswerMatrixMeasures.destinationWidth
    val h = AnswerMatrixMeasures.destinationHeight(questions)
    new MatOfPoint2f((0.0, 0.0), (w, 0.0), (w, h), (0.0, h))
  }

  def destinationSize(questions: Int) = {
    val contour = destinationContour(questions)
    val thirdCorner = contour.toArray()(2)
    new Size(thirdCorner.x, thirdCorner.y)
  }

  def cells(questions: Int): Seq[MatOfPoint] = {
    def xPositionOfCellColumn(column: Int) = column * (cellWidth + columnSpaceWidth)
    def yPositionOfCellRow(row: Int) = row * destinationHeight(questions) / rows(questions)

    if (false) {
      // COLUMNS
      for (c <- 0 until columns) yield {
        val x = xPositionOfCellColumn(c)
        val h = destinationHeight(questions)
        new MatOfPoint((x, 0.0), (x + cellWidth, 0.0), (x + cellWidth, h), (x, h))
      }
    }
    else {
      for (c <- 0 until columns; r <- 0 until rows(questions)) yield {
        val x = xPositionOfCellColumn(c) + cellHeaderWidth
        val h = destinationHeight(questions) / rows(questions)
        val y = yPositionOfCellRow(r)
        new MatOfPoint((x, y), (x + cellWidth - cellHeaderWidth, y), (x + cellWidth - cellHeaderWidth, y + h), (x, y + h))
      }

    }
  }
}