package imgproc

import org.opencv.core._

object AnswerMatrixMeasures {


  import imgproc.Implicits._

  val columns = 5
  val destinationWidth = 800.0
  val cellHeaderToHeaderWidthRatio = 0.7 / (0.7 + 1.3)
  val columnSpaceToCellWidthRatio = 0.15
  val matrixWithToTopOfQRRatio = 0.20
  val matrixWithToLeftOfQRRatio = 0.02
  val matrixWithToQRWidthRatio = 0.18


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
    new MatOfPoint((0.0, 0.0), (w, 0.0), (w, h), (0.0, h))
  }

  def fromMatrixToStudentInfoLocation( matrixLocation: MatOfPoint ) : MatOfPoint = {
    val points = matrixLocation.toArray
    val tl = points(0)
    val tr = points(1)
    val bl = points(3)
    val br = points(2)

    val matrixHeight = (bl - tl).withModulus( (bl - tl).modulus*(1 + AnswerMatrixMeasures.matrixWithToQRWidthRatio) )

    val xAxis = (tr - tl)
    val yAxis = new Point(-xAxis.y, xAxis.x)

    val topLeft = tl -
      (yAxis * AnswerMatrixMeasures.matrixWithToTopOfQRRatio) -
      (xAxis * AnswerMatrixMeasures.matrixWithToLeftOfQRRatio)
    val topRight = topLeft + (xAxis * (1+2*AnswerMatrixMeasures.matrixWithToLeftOfQRRatio ) )
    val bottomLeft = topLeft + (yAxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio) + matrixHeight
    val bottomRight = topRight + (yAxis * AnswerMatrixMeasures.matrixWithToQRWidthRatio) + matrixHeight

    new MatOfPoint(topLeft, topRight, bottomRight, bottomLeft)
  }

  def studentInfoDestinationContour(questions: Int) : MatOfPoint = {
    val sidc = fromMatrixToStudentInfoLocation(destinationContour(questions) ).toArray
    val topLeft = sidc(0)
    val topRight = sidc(1)
    val bottomRight = sidc(2)
    val bottomLeft = sidc(3)

    val origin = topLeft

    new MatOfPoint(topLeft-origin, topRight-origin, bottomRight-origin, bottomLeft-origin)
  }


  def destinationSize(questions: Int) = {
    val contour = destinationContour(questions)
    val thirdCorner = contour.toArray()(2)
    new Size(thirdCorner.x, thirdCorner.y)
  }

  def studentInfoDestinationSize(questions: Int) = {
    val contour = studentInfoDestinationContour(questions)
    val thirdCorner = contour.toArray()(2)
    new Size(thirdCorner.x, thirdCorner.y)
  }


  def cellsAsMatOfPoint(questions: Int): Seq[MatOfPoint] = {
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

  def cells(questions: Int): Seq[Rect] = {
    def xPositionOfCellColumn(column: Int) = column * (cellWidth + columnSpaceWidth)
    def yPositionOfCellRow(row: Int) = row * destinationHeight(questions) / rows(questions)

    for (c <- 0 until columns; r <- 0 until rows(questions)) yield {
      val x = xPositionOfCellColumn(c) + cellHeaderWidth
      val h = destinationHeight(questions) / rows(questions)
      val y = yPositionOfCellRow(r)
      val w = cellWidth - cellHeaderWidth
      new Rect(x.toInt, y.toInt, w.toInt, h.toInt)
    }
  }

  val cellArea = cells(1)(0).area()
}