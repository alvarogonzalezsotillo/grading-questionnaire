package imgproc

import org.opencv.core._

object AnswerMatrixMeasures {
  def apply(version: Int) = version match {
    case 0 => new AnswerMatrixMeasures()
    case 1 => new AnswerMatrixMeasures()
  }

  val matrixWithToLeftOfQRRatio = 0.02
  val matrixWithToTopOfQRRatio: Double = 0.20
  val matrixWithToQRWidthRatio: Double = 0.18


}

class AnswerMatrixMeasuresHorizontalLetter(val columns: Int = 5) {

  def rows(questions: Int) = (1.0 * questions / columns).ceil.toInt

  object TypeSafeWidthHeight {

    case class X(x: Double) {
      def +(ox: X) = X(x + ox.x)

      def *(r: Double) = X(x * r)
    }

    case class Y(y: Double) {
      def +(oy: Y) = Y(y + oy.y)

      def *(r: Double) = Y(y * r)
    }

    case class Point(x: X, y: Y) {
      def +(p: Point) = Point(x + p.x, y + p.y)

      def *(d: Double) = Point(x * d, y * d)
    }

    case class Width(w: Double) {
      def *(r: WidthToHeightRatio): Height = Height(w * r.r)

      def *(r: Double) = Width(w * r)

      def +(wi: Width) = Width(w + wi.w)

      def toX = X(w)
    }

    case class Height(h: Double) {
      def *(r: HeightToWidthRatio) = Width(h * r.r)

      def *(r: Double) = Height(h * r)

      def +(he: Height) = Height(h + he.h)

      def toY = Y(h)
    }

    case class Size(w: Width, h: Height) {
      def on(o: Point) = Rect(o, w, h)
    }

    case class Rect(o: Point, w: Width, h: Height)

    case class WidthToHeightRatio(r: Double)

    case class HeightToWidthRatio(r: Double)

  }
  import TypeSafeWidthHeight._


  object Params {
    val cellHeaderSize = Size(Width(60), Height(20))
    val answerCellAvailableWidth = Width(180)
    val cellHeaderToCellWidthGap = Width(5)
    val cellSize = Size(Width(150), cellHeaderSize.h)
    val answerTableOrigin = Point(X(0), Y(0))
  }

  import Params._

  val answerTableWidth = (cellHeaderSize.w + answerCellAvailableWidth) * columns

  def answerTableSize(questions: Int) = Size(answerTableWidth, cellHeaderSize.h * rows(questions))

  def answerCell(question: Int, questions: Int) = {
    assert(question >= 0 && question < questions)

    val rowOfQuestion = question / questions
    val columnOfQuestion = question % columns

    object LocalComputedAndCacheableValues {
      val basePointForCells = answerTableOrigin + Point((cellHeaderSize.w + cellHeaderToCellWidthGap).toX, Y(0))
      val columnOffset = Point((answerCellAvailableWidth + cellHeaderSize.w).toX, Y(0))
      val rowOffset = Point(X(0), cellSize.h.toY)
    }

    import LocalComputedAndCacheableValues._

    cellSize.on(basePointForCells + rowOffset * rowOfQuestion + columnOffset * columnOfQuestion)
  }

  def answerCells(questions: Int) = for (q <- 0 until questions) yield answerCell(q, questions)


}


class AnswerMatrixMeasures(vertical: Boolean = false) {

  import imgproc.Implicits._
  import AnswerMatrixMeasures._

  val columns = 5
  val destinationWidth = 800.0
  val cellHeaderToHeaderWidthRatio = (0.7 + 0.3) / (0.7 + 1.3)
  val columnSpaceToCellWidthRatio = 0.15


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
    val w = destinationWidth
    val h = destinationHeight(questions)
    new MatOfPoint((0.0, 0.0), (w, 0.0), (w, h), (0.0, h))
  }

  def fromMatrixToStudentInfoLocation(matrixLocation: MatOfPoint): MatOfPoint = {
    val points = matrixLocation.toArray
    val tl = points(0)
    val tr = points(1)
    val bl = points(3)
    val br = points(2)

    val matrixHeight = (bl - tl).withModulus((bl - tl).modulus * (1 + matrixWithToQRWidthRatio))

    val xAxis = (tr - tl)
    val yAxis = new Point(-xAxis.y, xAxis.x)

    val topLeft = tl -
      (yAxis * matrixWithToTopOfQRRatio) -
      (xAxis * matrixWithToLeftOfQRRatio)
    val topRight = topLeft + (xAxis * (1 + 2 * matrixWithToLeftOfQRRatio))
    val bottomLeft = topLeft + (yAxis * matrixWithToQRWidthRatio) + matrixHeight
    val bottomRight = topRight + (yAxis * matrixWithToQRWidthRatio) + matrixHeight

    new MatOfPoint(topLeft, topRight, bottomRight, bottomLeft)
  }

  def studentInfoDestinationContour(questions: Int): MatOfPoint = {
    val sidc = fromMatrixToStudentInfoLocation(destinationContour(questions)).toArray
    val topLeft = sidc(0)
    val topRight = sidc(1)
    val bottomRight = sidc(2)
    val bottomLeft = sidc(3)

    val origin = topLeft

    new MatOfPoint(topLeft - origin, topRight - origin, bottomRight - origin, bottomLeft - origin)
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


  def cells(questions: Int): Seq[MatOfPoint] = {
    def xPositionOfCellColumn(column: Int) = column * (cellWidth + columnSpaceWidth)
    def yPositionOfCellRow(row: Int) = row * cellHeight

    def cell(c: Int, r: Int) = {
      val x = xPositionOfCellColumn(c) + cellHeaderWidth
      val h = cellHeight
      val y = yPositionOfCellRow(r)
      val w = cellWidth - cellHeaderWidth
      new Rect(x.toInt, y.toInt, w.toInt, h.toInt).asShape
    }

    val ret = if (!vertical) {
      for (c <- 0 until columns; r <- 0 until rows(questions)) yield cell(c, r)
    }
    else {
      for (r <- 0 until rows(questions); c <- 0 until columns) yield cell(c, r)
    }

    ret.take(questions)
  }

  val cellHeight = destinationHeight(20) / rows(20)

  val cellArea = cellWidth * cellHeight
}
